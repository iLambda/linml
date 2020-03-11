
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
(* State manipulation *)

(** [default] returns the initial state of the interpreter *)
val default : unit -> state

(* ------------------------------------------------------------------------------- *)
(* Actions *)

(** [eval state input] evaluates [input], and then returns a new state *)
val eval : state -> string -> (state * string) Lwt.t