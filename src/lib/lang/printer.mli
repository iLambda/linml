open Types
open Terms
open Utils
open Utils.Atom

(* This module defines a pretty-printer for LinML.
   It relies on the basic machinery provided by
   [Export]. *)

(* Print an atom. *)
val print_atom: Export.env -> atom -> string

(* Print a type. *)
val print_type: Export.env -> ftype -> string

(* Print a program. *)
val print_program: ('a, 'b, 'c) _program -> string
