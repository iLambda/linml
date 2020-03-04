open Utils.Identifier

(* ------------------------------------------------------------------------- *)

(* Sorts. *)

(* We have four sorts of identifiers: term variables, type variables,
   data constructors, and type constructors. Each sort defines an
   independent namespace. *)

   let term_sort : sort = (0, "term")
   let type_sort : sort = (1, "type")
   let data_sort : sort = (2, "data constructor")
   let typecon_sort : sort = (3, "type constructor")