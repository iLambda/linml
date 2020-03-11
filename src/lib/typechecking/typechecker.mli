open Lang.Terms
open Lang.Types
open Lang.Kinds
open Utils

(* The type-checker checks that a complete program is well-typed. *)

(* Furthermore, the type-checker records typing information in various
   nodes, as explained in [Terms]. This is done by writing the
   references that exist at these nodes. *)

(* The type-checker returns the inferred type of the program, together
   with an export environment that allows printing this type if
   desired. *)

val program: pre_program -> Export.env * Env.env

val declaration: Export.env -> Env.env -> kind_table -> pre_declaration -> Export.env * Env.env * kind_table

(* [type_of t] is a constant time operation that exploits the metadata
   produced by the typechecker to return the type of any given
   petrified term. *)

val type_of: fterm -> ftype
