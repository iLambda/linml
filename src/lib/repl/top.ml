open React
open Lwt
open LTerm_key
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
  (* TODO : enter is newl *)
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
        
