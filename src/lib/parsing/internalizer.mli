open Lang
open Utils

(* This module turns external syntax ito internal syntax. In particular,
   this involves replacing identifiers with atoms, and making sure that
   every identifier is properly bound. *)

val program: Syntax.program -> Terms.pre_program

val declaration: Import.env -> Syntax.declaration -> Import.env * Terms.pre_declaration

val declarations: Import.env -> Syntax.declaration list -> Import.env * Terms.pre_declaration list