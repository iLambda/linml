open ANSITerminal
open Cmdliner
open Lang
open Parsing
open Typechecking
open Utils

(* ------------------------------------------------------------------------------- *)
(* Actual functions *)

(* Get lexbuf *)
let lexbuf filename = 
  (* Get channel *)
  let channel, name = match filename with 
    (* Read piped data *)
    | "" -> Stdlib.stdin, ""
    (* Read file *)
    | filename -> Stdlib.open_in filename, filename
  in
  (* Setup lexbuf *)
  let lexbuf = Lexing.from_channel channel in 
  (* Default its position *)
  lexbuf.lex_curr_p <- { 
    pos_fname = name; pos_lnum = 1; 
    pos_bol = 0; pos_cnum = 0
  }; 
  (* Return the lexbuf *)
  lexbuf

(* Get program *)
let program lexbuf syntax desugar = 
  (* Try parse *)
  let syn_program = 
    try Parser.program Lexer.main lexbuf
    with Parser.Error ->
      Error.errorb lexbuf "Syntax error.\n" in 
  (* If show, say syntax is ok *)
  if syntax && not desugar then printf [] "Syntax OK.\n";
  (* Internalize *)
  let program = Internalizer.program syn_program in 
  (* TODO: pretty print *)
  if desugar then printf [] "%s" (Terms.show_pre_program program);
  (* Return program *)
  program

(* Typecheck program *)
let typecheck preprogram typecheck = 
  (* Typecheck *)
  let export, ty = Typechecker.run preprogram in 
  (* Check if type printing *)
  if typecheck then printf [] "%s" (Printer.print_type export ty);
  (* Petrify *)
  let program = Terms.petrify preprogram in
  (* Return the petrified program, and type *)
  program, (export, ty)

(* ------------------------------------------------------------------------------- *)
(* Cmdliner options *)

(* Get the filename *)
let filename = 
  (* Documentation *)
  let doc = "The source file to be compiled. If not present, data from stdin will be read." in 
  (* Return the optional channel *)
  Arg.(value & pos 0 string "" & info [] ~docv:"FILENAME" ~doc)

(* Check if syntax check *)
let syntax = 
  (* Documentation *)
  let doc = "Prints a message if the program is syntactically correct." in 
  (* The cmd *)
  Arg.(value & flag & info ["s"; "syncheck"] ~docv:"SYNTAX CHECK" ~doc)

(* Check if desugar check *)
let desugar = 
  (* Documentation *)
  let doc = "Prints the program one syntactic sugar has been erased." in 
  (* The cmd *)
  Arg.(value & flag & info ["d"; "desugar"] ~docv:"DESUGAR" ~doc)

(* Check if typecheck *)
let typeprint = 
  (* Documentation *)
  let doc = "Prints the program's type." in 
  (* The cmd *)
  Arg.(value & flag & info ["t"; "type"] ~docv:"TYPE" ~doc)


(* ------------------------------------------------------------------------------- *)
(* Cmdliner terms *)

(* The lexbuf *)
let lexbuf_t = Term.(const lexbuf $ filename)
(* The pre-program *)
let preprogram_t = Term.(const program $ lexbuf_t $ syntax $ desugar)
(* The program *)
let program_t = Term.(const typecheck $ preprogram_t $ typeprint)

(* ------------------------------------------------------------------------------- *)
(* Cmdliner program info *)

let info =
  let doc = "LinML compiler" in
  let man = [
    `S Manpage.s_bugs;
    `P "Post bug reports to <github.com/iLambda/linml>." ]
  in
  Term.info "llbuild" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man

(* ------------------------------------------------------------------------------- *)
(* Entry point *)

let () = Term.exit @@ Term.eval (program_t, info)
