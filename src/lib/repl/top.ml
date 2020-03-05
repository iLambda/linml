open React
open Lwt
(* open LTerm_widget *)

let ( let* ) = Lwt.bind 

(* +-----------------------------------------------------------------+
   | Prompt and output wrapping                                      |
   +-----------------------------------------------------------------+ *)

(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

class read_line ~term ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_string.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    self#set_prompt (S.const (Ui.make_prompt term state))
end

(* ------------------------------------------------------------------------------- *)
(* Main loop *)

let rec loop term history state =
  Lwt.catch (fun () ->
    let rl = new read_line ~term ~history:(LTerm_history.contents history) ~state in
    rl#run >|= fun command -> Some command)
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
    let command_utf8= Zed_string.to_utf8 command in
    let state, out = Interpreter.eval state command_utf8 in
    LTerm.fprintls term (Ui.make_output term state out)
    >>= fun () ->
    LTerm_history.add history command;
    loop term history state
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
      let state = Interpreter.default in
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
        
