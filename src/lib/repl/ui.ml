open Array
open CamomileLibrary
open Interpreter
open LTerm_text
open LTerm_geom
open LTerm_style
open Printf

(* ------------------------------------------------------------------------------- *)
(* Helpers *)
let ( let* ) = Lwt.bind 
let (^^) = Array.append

(* ------------------------------------------------------------------------------- *)
(* Draw on screen *)


(** Create a prompt based on the current interpreter state *)
let make_prompt term state =
  (* Get temrinal size *)
  let size = LTerm.size term in
  (* Get time, and interpreter state *)
  let time = Unix.localtime (Unix.time ()) in
  (* Format *)
  let cmd_time = sprintf "%02d:%02d:%02d" time.Unix.tm_hour time.tm_min time.tm_sec in
  let cmd_id = sprintf "%d" state.n in
  
  (* Compute header *)
  let header_left = eval [ B_bold true; B_fg blue; S"─( "; B_fg cyan; S cmd_id ; B_fg blue; S" )─"; B_bold false ] in 
  let header_right = eval [ B_bold true; B_fg blue; S"─[ "; B_fg green; S cmd_time ; B_fg blue; S" ]─"; B_bold false ] in 
  let header = 
    if length header_left + length header_right > size.cols then
      sub (header_left ^^ header_right) 0 size.cols
    else
      concat 
        [ header_left;
          make
            (size.cols - length header_left - length header_right)
            (Zed_char.unsafe_of_uChar (UChar.of_int 0x2500), 
              { none with foreground = Some blue; bold = Some true });
          header_right; 
          of_utf8 "\n" ]
  in
  (* Start of line  *)
  let second_line = eval [ B_bold true; B_fg blue; S "lltop"; B_fg cyan; S " < " ] in
  (* Return *)
  header ^^ second_line
    
(** Format the interpreter output for REPL display *)
let make_output _term _state out =
  (* Start of line *)
  let header = eval [ B_bold true; B_fg red; S "lltop"; B_fg magenta; S " > " ] in
  (* Print output *)
  let output = of_utf8_maybe_invalid out in
  (* Return *)
  header ^^ output

(** Displays a greeting *)
let greet term = 
  (* Create a context to render the welcome message *)
  let size = LTerm.size term in
  let size = { rows = 3; cols = size.cols } in
  let matrix = LTerm_draw.make_matrix size in
  let ctx = LTerm_draw.context matrix size in

  (* Draw the message in a box. *)
  let message = sprintf "LLtop version %s (using LinML version %s)" "0.0.1" "0.0.1" in
  LTerm_draw.fill_style ctx LTerm_style.({ none with foreground = Some lblue });
  LTerm_draw.draw_hline ctx 0 0 size.cols LTerm_draw.Light;
  LTerm_draw.draw_frame ctx {
    row1 = 0;
    row2 = 3;
    col1 = (size.cols - (String.length message + 4)) / 2;
    col2 = (size.cols + (String.length message + 4)) / 2;
  } LTerm_draw.Light;
  LTerm_draw.draw_styled ctx 1 ((size.cols - String.length message) / 2) (eval [B_fg LTerm_style.cyan; S message]);

  (* Render to the screen. *)
  let* () = LTerm.print_box term matrix in
  (* Move to after the box. *)
  let* () = LTerm.fprint term "\n\n" in
  (* Flush *)
  LTerm.flush term