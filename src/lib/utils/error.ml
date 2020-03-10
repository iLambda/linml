open ANSITerminal
(* open Printf *)
open Lexing

exception InternalError

type error_mode = 
  Exit | Exception

type location =
    Lexing.position * Lexing.position

let pp_location oc _ = Format.fprintf oc "<>"

let exit_mode = ref Exit

let dummy =
  (Lexing.dummy_pos, Lexing.dummy_pos)

let mode m = exit_mode := m

let do_exit () = 
  flush_all ();
  match !exit_mode with 
    | Exit -> exit 1
    | Exception -> raise InternalError
  
let is_dummy (pos1, pos2) =
  pos1 == Lexing.dummy_pos && pos2 == Lexing.dummy_pos

let override loc1 loc2 =
  if is_dummy loc2 then loc1 else loc2

let print_location (pos1, pos2) =
  let file = pos1.pos_fname in
  let line = pos1.pos_lnum in
  let char1 = pos1.pos_cnum - pos1.pos_bol in
  let char2 = pos2.pos_cnum - pos1.pos_bol in (* intentionally [pos1.pos_bol] *)
  (* Print as bold *)
  match file with 
    | "" -> eprintf [ Bold ] "Line %d, characters %d-%d:\n" line char1 char2
    | _  -> eprintf [ Bold ] "File \"%s\", line %d, characters %d-%d:\n" file line char1 char2
  (* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)

let signaled =
  ref false

let atoms2locs atoms =
  List.map (fun a ->
    let id = Atom.identifier a in
    Identifier.startp id, Identifier.endp id
  ) atoms

let warning locs message =
  List.iter print_location locs;
  (* Print error *)
  eprintf [ Bold; Foreground Red ] "Error";
  eprintf [] ": ";
  eprintf [] "%s%!" message;
  (* Flush stdout *)
  flush stdout

let signal locs message =
  warning locs message;
  signaled := true

let signala atoms message =
  signal (atoms2locs atoms) message

let error locs message =
  signal locs message;
  do_exit ()

let errora atoms message =
  error (atoms2locs atoms) message

let errorb lexbuf msg =
  error [ (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) ] msg

let signaled () =
  if !signaled then
    do_exit ()

let fail = do_exit 