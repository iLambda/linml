open Types
open Utils.Atom

(* ------------------------------------------------------------------------------- *)
(* Types *)

(* Table containing all the datatype & their constructors *)
type datatype_table

(* A type ctor *)
type datatype_ctor = 
  | Constructor of
      atom *          (* The name of the constructor *) 
      ftype           (* The type of the constructor *)

(* A type *)
type datatype =
  | DtyInductive of datatype_ctor list    (* Inductive *) 

  
(* ------------------------------------------------------------------------------- *)
(* Constants *)

(* An empty datatype table *)
val empty : datatype_table

(* ------------------------------------------------------------------------------- *)
(* Datatype table manipulation *)

(* Register a new type *)