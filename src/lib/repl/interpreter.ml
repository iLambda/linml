
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
  let out = "evaluated " ^ s in
  let new_state = { n = state.n + 1 } in
  (new_state, out)