open Types
open Utils.Atom

(* ------------------------------------------------------------------------------- *)
(* Types *)

(* Table containing all the datatypes & their constructors *)
type datatype_table = {
  (* The types *)
  dtys: datatype AtomMap.t;
  (* The constructors *)
  ctors: AtomSet.t
}

(* A type ctor *)
and datatype_ctor = 
  | Constructor of
      atom *          (* The name of the constructor *) 
      ftype           (* The type of the constructor *)

(* A type *)
and datatype =
  (* Inductive *) 
  | DtyInductive of 
      datatype_ctor list   (* Its ctors *)

  
(* ------------------------------------------------------------------------------- *)
(* Constants *)

(* An empty datatype table *)
let empty : datatype_table = { 
  dtys=AtomMap.empty; 
  ctors=AtomSet.empty; 
}
  
(* ------------------------------------------------------------------------------- *)
(* Datatype table manipulation *)

(* Register a new type *)