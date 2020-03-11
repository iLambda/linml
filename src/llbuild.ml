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
let program lexbuf syntax desugar typecheck = 
  (* Try parse *)
  let syn_program = 
    try Parser.program Lexer.main lexbuf
    with Parser.Error ->
      Error.errorb lexbuf "Syntax error.\n" in 
  (* If show, say syntax is ok *)
  if syntax && not desugar then printf [] "Syntax OK.\n";
  (* Internalize *)
  let preprogram = Internalizer.program syn_program in 
  (* TODO: pretty print *)
  if desugar then printf [] "%s" (Terms.show_pre_program preprogram);
  (* Typecheck *)
  let export, env = Typechecker.program preprogram in 
  (* Check if type printing *)
  if typecheck then printf [] "%s" (Env.print env export);
  (* Petrify *)
  let program = Terms.petrify preprogram in
  (* Return the petrified program, and type *)
  ignore (program, (export, env))

(* A toplevel iteration *)
let declaration lexbuf ienv xenv env ktable = 
  (* Try get a declaration *)
  let syn_decl = 
    try Parser.declaration Lexer.main lexbuf
    with Parser.Error ->
      Error.errorb lexbuf "Syntax error.\n"
  in 
  (* Internalize *)
  let ienv, pre_decl = Internalizer.declaration ienv syn_decl in
  (* Typecheck *)
  let xenv, env, ktable = Typechecker.declaration xenv env ktable pre_decl in
  (* Print env *)
  printf [] "%s\n" (Env.print ~color:false env xenv);
  flush stdout;
  (* Return *)
  ienv, xenv, env, ktable

(* Make toplevel mode *) 
let top lexbuf ctrlchar =
  (* Set exit mode of Error module as exception *)
  Error.mode Error.Exception;
  (* Save the environments *)
  let ienv = ref Import.empty in
  let xenv = ref Export.empty in 
  let tyenv = ref Env.empty in
  let ktable = ref Kinds.empty in
  (* Forever : *)
  while true do 
    (* Clear lexbuf *)
    Lexing.flush_input lexbuf;
    (* Try get a declaration *)
    begin try 
      (* Parse *)
      let new_ienv, new_xenv, new_tyenv, new_ktable = 
        declaration lexbuf !ienv !xenv !tyenv !ktable in 
      (* Save *)
      ienv := new_ienv; xenv := new_xenv;
      ktable := new_ktable; tyenv := new_tyenv
    (* Internal error occured ; break *)
    with Error.InternalError -> () end;
    (* Post a EOT on stderr and stdout *)
    if ctrlchar then begin 
      prerr_char '\x04'; print_char '\x04'; flush_all ()
    end
  done


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
  Arg.(value & flag & info ["ty"; "type"] ~docv:"TYPE" ~doc)

(* Check if binary stop character needed *)
let toplevel = 
  (* Documentation *)
  let doc = "Outputs control characters for lltop. Only works in toplevel mode." in 
  (* The cmd *)
  Arg.(value & flag & info ["lltop"] ~docv:"LLTOP" ~doc)


(* ------------------------------------------------------------------------------- *)
(* Cmdliner terms *)

(* The lexbuf *)
let lexbuf_t = Term.(const lexbuf $ filename)
(* The program mode *)
let program_t = Term.(const program $ lexbuf_t $ syntax $ desugar $ typeprint)
let toplevel_t = Term.(const top $ lexbuf_t $ toplevel)
(* The entrypoint *)
(* let main_ty = Term.(const main $ toplevel) *)

(* ------------------------------------------------------------------------------- *)
(* Cmdliner choices *)
let program_choice = 
  (* Make info for this choice *)
  let program_info = Term.info "program" in 
  (* Return term & info *)
  program_t, program_info

let toplevel_choice = 
  (* Make info for this choice *)
  let toplevel_info = Term.info "top" in 
  (* Return term & info *)
  toplevel_t, toplevel_info

let choices = [program_choice; toplevel_choice]



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

let () = Term.exit @@ Term.eval_choice (program_t, info) choices
(* let () = Term.exit @@ Term.eval (main_ty, info) *)
