open Printf
open Parsing
(* open Lang *)
open Typechecking
open Utils

let filename = ref None
let usage = sprintf "Usage: %s <options> <filename>\n" Sys.argv.(0)

let read filename : Syntax.program =
  let lexbuf = Lexing.from_channel (open_in filename) in
  lexbuf.lex_curr_p <- { 
    pos_fname = filename; pos_lnum = 1; 
    pos_bol = 0; pos_cnum = 0
  };
  try
    Parser.program Lexer.main lexbuf
  with Parser.Error ->
    Error.errorb lexbuf "Syntax error.\n"

let process filename = 
  filename
  |> read
  |> (fun p -> print_string (Syntax.show_program p); p)
  |> Parsing.Internalizer.program
  (* |> (fun p -> print_string (Lang.Terms.show_pre_program p); p) *)
  |> Typechecker.run
  (* |> (fun (_, t) -> (Lang.Types.pp_ftype Format.std_formatter t); t) *)
  |> (fun (_ , t) -> print_string (Lang.Printer.print_type (Export.empty) t); t)

  (* Discard *)
  |> ignore

let () = 
  (* Parse arguments *)
  Arg.parse (Arg.align []) (fun name -> filename := Some name) usage;
  (* Parse filename *)
  let filename = match !filename with 
    | Some filename -> filename
    | None -> fprintf stderr "%s%!" usage; exit 1
  in
  (* Set backtrace *)
  Printexc.record_backtrace true;
  (* Process the filename *)
  ignore(process filename)