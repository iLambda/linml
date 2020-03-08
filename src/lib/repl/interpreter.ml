open Lwt

let ( let* ) = bind

(* ------------------------------------------------------------------------------- *)
(* Types *)

type state = { 
  (* The number of input commands *)  
  n : int 
}

(* ------------------------------------------------------------------------------- *)
(* State manipulation *)

(** [default] returns the initial state of the interpreter *)
let default = { n=1 }

(* ------------------------------------------------------------------------------- *)
(* Actions *)

(** [eval state input] evaluates [input], and then returns a new state *)
let eval state s =
  (* TODO : good cmd *)
  let cmd = Lwt_process.shell "dune exec ./src/llbuild.exe -- -t" in
  (* Open a process *)
  let process = Lwt_process.open_process_full cmd in 
  (* Push program in stdout *)
  let* () = Lwt_io.write process#stdin s in
  let* () = Lwt_io.flush process#stdin in
  let* () = Lwt_io.close process#stdin in
  (* Wait for return status *)
  let* return_code = process#status in
  (* Check return code for output *)
  let output_channel = match return_code with 
    (* Exited with success *)
    | Unix.WEXITED 0 -> process#stdout
    (* Error *)
    | _ -> process#stderr
  in
  (* Read all from output channel *)
  let stream = Lwt_io.read_chars output_channel in 
  let* output = Lwt_stream.to_string stream in 
  (* Modify state *)
  let new_state = { n = state.n + 1 } in
  (* Return output *)
  Lwt.return (new_state, output)