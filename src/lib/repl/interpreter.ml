open Lwt

let ( let* ) = bind

(* ------------------------------------------------------------------------------- *)
(* Types *)

type state = { 
  (* The number of input commands *)  
  n : int;
  (* The process *)
  process: Lwt_process.process_full;
  (* The response stream *)
  response: string Lwt_stream.t;
}

(* ------------------------------------------------------------------------------- *)
(* Constants *)

let command = Lwt_process.shell "dune exec ./src/llbuild.exe top -- --lltop"

(* ------------------------------------------------------------------------------- *)
(* Helpers *)
let extract_till_x04 buffer stream = 
  (* Get the data from the stream, and throw the \x04 *)
  let* data = Lwt_stream.get_while (fun c -> c <> '\x04') stream in 
  let* () = Lwt_stream.junk stream in 
  (* Append to buffer *)
  return (List.iter (Buffer.add_char buffer) data)

(* ------------------------------------------------------------------------------- *)
(* State manipulation *)


(** [default] returns the initial state of the interpreter *)
let default () = 
  (* Open the process *)
  let process = Lwt_process.open_process_full command in 
  (* Make response stream *)
  let buffer = Buffer.create 32 in
  let response = 
    (* Get char stream from process stdout & stderr *)
    let stdout_stream = Lwt_io.read_chars process#stdout in
    let stderr_stream = Lwt_io.read_chars process#stderr in
    (* Make a new strea *)
    Lwt_stream.from (fun () -> 
      (* Clear buffer *)
      Buffer.clear buffer;
      (* Get stdout while not \x04 *)
      let* () = extract_till_x04 buffer stdout_stream in 
      let* () = extract_till_x04 buffer stderr_stream in  
      (* Return *)
      return_some (Buffer.contents buffer))
  in
  (* Make default state *)
  { 
    (* Number of input commands *)
    n=1;
    (* The process & response stream*)
    process; response
  }

(* ------------------------------------------------------------------------------- *)
(* Actions *)

(** [eval state input] evaluates [input], and then returns a new state *)
let eval state input =
  (* Write the program into stdout *)
  let* () = Lwt_io.write state.process#stdin input in
  let* () = Lwt_io.flush state.process#stdin in
  (* Read all from stdout/stderr *)
  let* output = Lwt_stream.next state.response in
  (* Modify state *)
  let new_state = { n = state.n + 1; process = state.process; response = state.response } in
  (* Return output *)
  Lwt.return (new_state, output)