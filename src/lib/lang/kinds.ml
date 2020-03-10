open Types
open Utils.Atom

(* ------------------------------------------------------------------------------- *)
(* Types *)

(* Table containing all the kinds (tyctor, datactor) *)
type kind_table = {
  (* The type constructors *)
  ty_cons: datatype AtomMap.t;
  (* The data constructors *)
  data_cons: AtomSet.t
}

(* A type ctor *)
and datatype_ctor = 
  | Constructor of
      atom *          (* The name of the constructor *) 
      ftype           (* The type of the constructor *)

(* A type *)
and datatype =
  | DtyInductive of datatype_ctor list    (* Inductive *) 
  
(* ------------------------------------------------------------------------------- *)
(* Constants *)

(* An empty datatype table *)
let empty : kind_table = { 
  ty_cons=AtomMap.empty; 
  data_cons=AtomSet.empty; 
}
  
(* ------------------------------------------------------------------------------- *)
(* Datatype table manipulation *)

(* Register a new type *)