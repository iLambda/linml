
(* ------------------------------------------------------------------------------- *)
(* Types *)

type state = { 
  (* The number of input commands *)  
  n : int 
}

(* ------------------------------------------------------------------------------- *)
(* State manipulation *)

(** [default] returns the initial state of the interpreter *)
val default : state

(* ------------------------------------------------------------------------------- *)
(* Actions *)

(** [eval state input] evaluates [input], and then returns a new state *)
val eval : state -> string -> state * string