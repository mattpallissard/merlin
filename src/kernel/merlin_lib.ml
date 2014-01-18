open Std
open Misc

module Lexer: sig
  type keywords = Raw_lexer.keywords

  (* Lexing is split in two steps.

     First the list of tokens is represented by a [item History.t].
     It's a pure value, independent of the context.

     Second the process of lexing is represented by values of type [t].  You
     resume the process from an arbitrary list of tokens, feeding it with one
     or more string, and you can extract the current list of tokens and cursor
     position at any time.
     Beware, the cursor may be in the middle of a not yet determined token.

     The process ultimately ends when fed with the empty string, representing
     EOF.
  *)

  (* Lexing step *)
  type item =
    | Valid of Lexing.position * Raw_parser.token * Lexing.position
    | Error of Raw_lexer.error * Location.t
  val item_start: item -> Lexing.position
  val item_end: item -> Lexing.position

  (** Create an empty list new lexer *)
  val empty: filename:string -> item History.t

  (** Prepare for lexing.
      Returns the start position (end position of last valid token), and a
      lexing function that will append at most one token to the history at each
      call. *)
  type t
  val history: t -> item History.t
  val start: keywords -> item History.t -> t
  val position: t -> Lexing.position
  val feed: t -> string -> bool
  val eof: t -> bool
end = struct
  type keywords = Raw_lexer.keywords

  (* Lexing step *)
  type item =
    | Valid of Lexing.position * Raw_parser.token * Lexing.position
    | Error of Raw_lexer.error * Location.t

  let item_start (Valid (p,_,_) | Error (_,{Location. loc_start = p})) =
    p

  let item_end (Valid (_,_,p) | Error (_,{Location. loc_end = p})) =
    p

  (** Create an empty list new lexer *)
  let empty ~filename =
    let pos =
      { Lexing.
        pos_fname = filename;
        pos_lnum  = 1;
        pos_bol   = 0;
        pos_cnum  = 0;
      }
    in
    History.initial (Valid (pos, Raw_parser.ENTRYPOINT, pos))

  type t = {
    (* Result *)
    mutable history: item History.t;
    (* Input buffer *)
    refill: string option ref; (* Input not yet sent to lexer *)
    refill_empty: bool ref;    (* Lexer internal buffer status *)
    (* Lexer data *)
    state: Raw_lexer.state;
    lexbuf: Lexing.lexbuf;
    mutable resume: (unit -> Raw_parser.token Raw_lexer.result) option;
  }

  let history t = t.history

  (** Prepare for lexing.
      Returns the start position (end position of last valid token), and a
      lexing function that will append at most one token to the history at each
      call. *)
  let make_lexbuf empty refill position =
    Lexing.from_strings ~position ~empty ""
      (fun () ->
         match !refill with
         | Some s -> refill := None; s
         | None -> "")

  let start keywords history =
    let position = match History.focused history with
      | Valid (_,_,p) -> p
      | Error (_,l) -> l.Location.loc_end
    in
    let refill = ref None in
    let refill_empty = ref true in
    let lexbuf = make_lexbuf refill_empty refill position in
    {
      history;
      state = Raw_lexer.make keywords;
      resume = None; refill; refill_empty; lexbuf;
    }

  let position t = Lexing.immediate_pos t.lexbuf

  let feed t str =
    if not t.lexbuf.Lexing.lex_eof_reached then begin
      t.refill := Some str;
      let append item =
        t.history <- History.insert item t.history
      in
      let rec aux = function
        (* Lexer interrupted, there is data to refill: continue. *)
        | Raw_lexer.Refill f
          when !(t.refill) <> None || not !(t.refill_empty) ->
          aux (f ())
        (* Lexer interrupted, nothing to refill, return to caller. *)
        | Raw_lexer.Refill r ->
          t.resume <- Some r
        (* EOF Reached: notify EOF to parser, stop now *)
        | Raw_lexer.Return Raw_parser.EOF ->
          begin match History.focused t.history with
            | Valid (_,Raw_parser.EOF,_) -> ()
            | _ ->
              append (Valid (t.lexbuf.Lexing.lex_start_p,
                             Raw_parser.EOF,
                             t.lexbuf.Lexing.lex_curr_p));
          end
        | Raw_lexer.Return token ->
          append (Valid (t.lexbuf.Lexing.lex_start_p,
                         token,
                         t.lexbuf.Lexing.lex_curr_p));
          continue ()
        | Raw_lexer.Error (e,l) ->
          append (Error (e,l));
          continue ()
      and continue () =
        aux (Raw_lexer.token t.state t.lexbuf)
      in
      begin match t.resume with
      | Some f ->
        t.resume <- None;
        aux (f ())
      | None -> continue ()
      end;
      true
    end
    else
      false

  let eof t = t.lexbuf.Lexing.lex_eof_reached
end

(* Project configuration *)
module Project : sig
  type t

  (** Create a new project *)
  val create: unit -> t

  (* Current buffer path *)
  val set_local_path : t -> string list -> unit

  (* Project-wide configuration *)
  val set_dot_merlin : t -> Dot_merlin.config option -> [`Ok | `Failures of (string * exn) list]

  (* Config override by user *)
  module User : sig
    val reset : t -> unit
    val path : t -> action:[`Add|`Rem] -> var:[`Build|`Source] -> ?cwd:string -> string -> unit
    val load_packages : t -> string list -> [`Ok | `Failures of (string * exn) list]
    val set_extension : t -> enabled:bool -> string -> unit
  end

  (* Path configuration *)
  val source_path : t -> Path_list.t
  val build_path  : t -> Path_list.t
  val cmt_path    : t -> Path_list.t

  (* List all top modules of current project *)
  val global_modules : t -> string list

  (* Force recomputation of top modules *)
  val flush_global_modules : t -> unit

  (* Enabled extensions *)
  val extensions: t -> Extension.set

  (* Lexer keywords for current config *)
  val keywords: t -> Lexer.keywords

  (* Make global state point to current project *)
  val setup : t -> unit

end = struct

  type config = {
    mutable cfg_extensions : Extension.set;
    mutable cfg_flags     : string list list;
    cfg_path_build  : string list ref;
    cfg_path_source : string list ref;
    cfg_path_cmi    : string list ref;
    cfg_path_cmt    : string list ref;
    cfg_path_pkg    : string list ref;
  }

  let empty_config () = {
    cfg_extensions = String.Set.empty;
    cfg_flags = [];
    cfg_path_build  = ref [];
    cfg_path_source = ref [];
    cfg_path_cmi    = ref [];
    cfg_path_cmt    = ref [];
    cfg_path_pkg    = ref [];
  }

  let reset_config cfg =
    cfg.cfg_path_build := [];
    cfg.cfg_path_source := [];
    cfg.cfg_path_cmi := [];
    cfg.cfg_path_cmt := [];
    cfg.cfg_path_pkg := []

  type t = {
    dot_config : config;
    user_config : config;

    mutable flags : Clflags.set;
    mutable warnings : Warnings.set;

    local_path : string list ref;

    source_path : Path_list.t;
    build_path  : Path_list.t;
    cmt_path    : Path_list.t;

    mutable global_modules: string list option;
    mutable keywords_cache : Lexer.keywords * Extension.set;
  }

  let flush_global_modules project =
    project.global_modules <- None

  let global_modules project =
    match project.global_modules with
    | Some lst -> lst
    | None ->
      let lst =
        Misc.modules_in_path ~ext:".cmi"
          (Misc.Path_list.to_strict_list project.build_path)
      in
      project.global_modules <- Some lst;
      lst

  let create () =
    let dot_config = empty_config () in
    let user_config = empty_config () in
    let local_path = ref [] in
    dot_config.cfg_extensions <- Extension.default;
    let prepare l = Path_list.(of_list (List.map ~f:of_string_list_ref l)) in
    { dot_config; user_config;
      flags = Clflags.copy Clflags.initial;
      warnings = Warnings.copy Warnings.initial;
      local_path;
      source_path = prepare [
          user_config.cfg_path_source;
          dot_config.cfg_path_source;
          local_path;
        ];
      build_path = prepare [
          user_config.cfg_path_cmi;
          user_config.cfg_path_build;
          dot_config.cfg_path_cmi;
          dot_config.cfg_path_build;
          user_config.cfg_path_pkg;
          dot_config.cfg_path_pkg;
          local_path;
        ];
      cmt_path = prepare [
          user_config.cfg_path_cmt;
          dot_config.cfg_path_cmt;
          user_config.cfg_path_build;
          dot_config.cfg_path_build;
          user_config.cfg_path_pkg;
          dot_config.cfg_path_pkg;
          local_path;
        ];
      global_modules = None;
      keywords_cache = Raw_lexer.keywords [], String.Set.empty;
    }

  let source_path p = p.source_path
  let build_path  p = p.build_path
  let cmt_path    p = p.cmt_path

  let set_local_path project path =
    if path != !(project.local_path) then begin
      project.local_path := path;
      flush_global_modules project
    end

  let set_dot_merlin project dm =
    let module Dm = Dot_merlin in
    let dm = match dm with | Some dm -> dm | None -> Dm.empty_config in
    let cfg = project.dot_config in
    let result, path_pkg = Dot_merlin.path_of_packages dm.Dm.packages in
    cfg.cfg_path_pkg := List.filter_dup (path_pkg @ !(cfg.cfg_path_pkg));
    cfg.cfg_path_build := dm.Dm.build_path;
    cfg.cfg_path_source := dm.Dm.source_path;
    cfg.cfg_path_cmi := dm.Dm.cmi_path;
    cfg.cfg_path_cmt := dm.Dm.cmt_path;
    cfg.cfg_flags <- dm.Dm.flags;
    cfg.cfg_extensions <-
      String.Set.(union Extension.default (of_list dm.Dm.extensions));
    flush_global_modules project;
    result

  (* Config override by user *)
  module User = struct
    let reset project =
      let cfg = project.user_config in
      cfg.cfg_path_build := [];
      cfg.cfg_path_source := [];
      cfg.cfg_path_cmi := [];
      cfg.cfg_path_cmt := [];
      cfg.cfg_flags <- [];
      cfg.cfg_extensions <- String.Set.empty

    let path project ~action ~var ?cwd path =
      let cfg = project.user_config in
      let r = match var with
        | `Source -> cfg.cfg_path_source
        | `Build  -> cfg.cfg_path_build
      in
      let d = canonicalize_filename ?cwd
                (expand_directory Config.standard_library path)
      in
      r := List.filter ~f:((<>) d) !r;
      begin match action with
      | `Add -> r := d :: !r
      | `Rem -> ()
      end;
      flush_global_modules project

    let load_packages project pkgs =
      let result, path = Dot_merlin.path_of_packages pkgs in
      let rpath = project.user_config.cfg_path_pkg in
      rpath := List.filter_dup (path @ !rpath);
      result

    let set_extension project ~enabled path =
      let cfg = project.user_config in
      let f = String.Set.(if enabled then add else remove) in
      cfg.cfg_extensions <- f path cfg.cfg_extensions
  end

  (* Make global state point to current project *)
  let setup project =
    Config.load_path := project.build_path

  (* Enabled extensions *)
  let extensions project =
    String.Set.union
      project.dot_config.cfg_extensions
      project.user_config.cfg_extensions

  (* Lexer keywords for current config *)
  let keywords project =
    let set = extensions project in
    match project.keywords_cache with
    | kw, set' when String.Set.equal set set' -> kw
    | _ ->
      let kw = Extension.keywords set in
      project.keywords_cache <- kw, set;
      kw
end

module Parser = Merlin_parser

module Typer = Merlin_typer

module Buffer : sig
  type t
  val create: ?path:string -> Project.t -> Parser.state -> t

  val lexer: t -> Lexer.item History.t
  val update: t -> Lexer.item History.t -> unit
  val start_lexing: t -> Lexer.t

  val parser: t -> Parser.t
  val path: t -> Parser.path
  val typer: t -> Env.t * Typedtree.structure list
end = struct
  type t = {
    kind: Parser.state;
    path: string option;
    project : Project.t;
    mutable keywords: Lexer.keywords;
    mutable lexer: Lexer.item History.t;
    mutable parser: (Lexer.item * Parser.t) History.t;
    mutable typer: Merlin_typer.t;
    mutable parser_path: Parser.Path.t;
    env: Env.cache;
    btype: Btype.cache;
  }

  let initial_step kind token =
    let input = match token with
      | Lexer.Valid (s,t,e) -> s,t,e
      | _ -> assert false
    in
    (token, Parser.from kind input)

  let create ?path project kind =
    let path, filename = match path with
      | None -> None, "*buffer*"
      | Some path -> Some (Filename.dirname path), Filename.basename path
    in
    let lexer = Lexer.empty ~filename in
    Project.setup project;
    {
      path; project; lexer; kind;
      keywords = Project.keywords project;
      env = Env.new_cache ();
      parser = History.initial (initial_step kind (History.focused lexer));
      parser_path = Parser.Path.empty;
      btype = Btype.new_cache ();
      typer = Merlin_typer.empty ();
    }

  let setup buffer =
    Project.setup buffer.project;
    begin match buffer.path with
      | Some path -> Project.set_local_path buffer.project [path]
      | None -> ()
    end;
    Env.set_cache buffer.env;
    Btype.set_cache buffer.btype

  let lexer b = b.lexer
  let parser b = snd (History.focused b.parser)

  let path b =
    let parser_path = Parser.Path.update' (parser b) b.parser_path in
    b.parser_path <- parser_path;
    Parser.Path.get parser_path

  let typer b =
    setup b;
    let typer = Merlin_typer.update' () (parser b) b.typer in
    b.typer <- typer;
    Merlin_typer.value typer

  let update t l =
    t.lexer <- l;
    t.parser <- History.sync
        ~check:(fun token (token',_) -> token == token')
        ~init:(initial_step t.kind)
        ~fold:(fun token (_,parser) ->
            let result =
              match token with
              | Lexer.Error _ -> (token, parser)
              | Lexer.Valid (s,t,e) ->
                Logger.debugf `internal
                  (fun ppf t -> Format.fprintf ppf "received %s"
                      (Parser.Values.Token.to_string t))
                  t;
                match Parser.feed (s,t,e) parser with
                | `Accept _ ->
                  Logger.debug `internal "parser accepted";
                  (token, parser)
                | `Reject _ ->
                  Logger.debug `internal "parser rejected";
                  (token, parser)
                | `Step parser ->
                  token, parser
            in
            Logger.debugf `internal Parser.dump (snd result);
            result
          )
        t.lexer
        (Some t.parser)

  let start_lexing b =
    let kw = Project.keywords b.project in
    if kw != b.keywords then begin
      b.keywords <- kw;
      ignore (update b (History.drop_tail (History.seek_backward
                                             (fun _ -> true) b.lexer)))
    end else begin
      ignore (update b (History.seek_backward
                          (function ( Lexer.Valid (_,Raw_parser.EOF,_)
                                    | Lexer.Error _) -> true
                                  | _ -> false)
                          b.lexer))
    end;
    Lexer.start kw b.lexer
end

