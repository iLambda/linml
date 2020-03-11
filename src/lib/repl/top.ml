open CamomileLibrary
open React
open Lwt
(* open LTerm_widget *)

let ( let* ) = Lwt.bind 

let contains_terminator rope = 
  Zed_utf8.contains 
    (Zed_string.to_utf8 (Zed_rope.to_string rope)) 
    ";;" 

let before_terminator rope = 
  let pos = ref 0 in 
  let str = Zed_string.to_utf8 (Zed_rope.to_string rope) in
  (* Go through *)
  for i=0 to (Zed_utf8.length str) - 2 do 
    if UChar.eq (Zed_utf8.get str i) (UChar.of_char ';') 
       && UChar.eq (Zed_utf8.get str (i + 1)) (UChar.of_char ';')
    then pos := i;
  done;
  (* Cut&return*)
  let cmd = Zed_utf8.before str !pos in
  Zed_rope.of_string (Zed_string.of_utf8 cmd)

let newline = Zed_rope.of_string (Zed_string.of_utf8 "\n")
let separator = Zed_rope.of_string (Zed_string.of_utf8 ";;")

(* +-----------------------------------------------------------------+
   | Prompt and output wrapping                                      |
   +-----------------------------------------------------------------+ *)

(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

class read_line ~term ~history ~state = object(self)
(* inherit LTerm_read_line.read_line ~history () *)
  inherit [Zed_string.t] LTerm_read_line.engine ~history () as super
  inherit [Zed_string.t] LTerm_read_line.term term as super_term

  method! show_box = false

  (* The typed sentences *)
  val mutable sentences : Zed_rope.t option = None

  (* Evaluate *)
  method eval = Zed_rope.to_string (Option.get sentences)

  (* Send action *)
  method! send_action action = super#send_action action 

  (* Execute *)
  method! exec = function 
    (* Accept *)
    | Accept::actions -> 
        (* Get input *)
        let input = Zed_edit.text self#edit in 
        (* Check if ;; appears *)
        if contains_terminator input then begin 
          (* Cut the string before terminator, then append it *)
          let cut = Zed_rope.append (before_terminator input) separator in 
          let sanitized = 
            match sentences with 
              | None -> cut
              | Some sentences -> Zed_rope.concat newline [sentences; cut] 
            in
          (* Append this string, we're done *)
          return (Zed_rope.to_string (sanitized))
        end 
        else begin 
          (* Append *)
          sentences <- Some 
            (match sentences with 
              | None -> input
              | Some sentences -> Zed_rope.concat newline [sentences; input]);
          (* Input not finished, continue. *)
          self#insert (UChar.of_char '\n');
          self#exec actions
        end
    
        (* super_term#exec actions *)
        
    (* Default *)
    | actions -> super_term#exec actions

  initializer
    self#set_prompt (S.const (Ui.make_prompt term state));
end

(* ------------------------------------------------------------------------------- *)
(* Main loop *)

let rec loop term history state =
  (* Try get a command *)
  let* cmd =
    (* Catch errors *)
    Lwt.catch 
      (* Body *)
      (fun () ->
        let rl = new read_line ~term ~history:(LTerm_history.contents history) ~state in
        rl#run >|= fun command -> Some command)
      (* Error *)
      (function
        | Sys.Break -> return None
        | exn -> Lwt.fail exn) 
  in
  (* Check returned command *) 
  match cmd with 
    (* A command was found *)
    | Some command ->
      let command_utf8= Zed_string.to_utf8 command in
      let* state, out = Interpreter.eval state command_utf8 in
      let txt = Ui.make_output term state out in
      let* () = LTerm.fprintls term txt in
      LTerm_history.add history command;
      loop term history state
    (* No command *)
    | None ->
      loop term history state

(* ------------------------------------------------------------------------------- *)
(* Entry point *)

let run () =
  (* Load key bindings *)
  let* () = LTerm_inputrc.load () in 
  (* Run while catching errors *)
  Lwt.catch 
    (* To run *)
    (function () -> 
      (* Make interpreter default state & pipe stdout *)
      let state = Interpreter.default () in
      let* term = Lazy.force LTerm.stdout in
      (* Display a welcome message. *)
      let* () = Ui.greet term in
      (* Run loop *)
      loop term (LTerm_history.create []) state)
    (* Exception handler *)
    (function
      (* Interrupt read ! Quit *)
      | LTerm_read_line.Interrupt -> Lwt.return ()
      (* Failure, return exception *)
      | exn -> Lwt.fail exn)
        
