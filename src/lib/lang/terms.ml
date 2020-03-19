open Kinds
open Types
open Utils.Error
open Utils.Atom

(* A runtime contains mutable metadata.
   When petrified, it is forced, then turned
   into the underlying value  *)
type 'a runtime = 'a option ref
[@@deriving show]

let force (r: 'a runtime) : 'a = 
  match !r with
    | None -> failwith "Empty runtime !"
    | Some x -> x

let reset () : 'a runtime = ref None

(* Typechecker metadata *)
type type_metadata = ftype

and application_metadata = {
  (* The domain of the function *)
  domain: ftype;
  (* The codomain of the function *)
  codomain: ftype;
  (* Was the function banged ? *)
  bang: bool;
}

and type_application_metadata = {
  (* The abstraction context *)
  context: ftype_context; [@opaque]
}

[@@deriving show]

(* Terms *)
type ('a, 'b, 'c, 'd) _fterm = 
  (*
   *  LEAFS
   *)
  (* Constant *)
  | TeConst of constant 
  (* x *)
  | TeVar of 
      atom *  (* Variable name *)
      'a      (* Metadata (type of variable) *)
  (* Dtycon *)
  | TeData of 
      atom *  (* Data ctor name *)
      'a      (* Metadata (type of the ctor) *)
  (* 
   *  CONSTRUCTS
   *)
  (* t, t' *)
  | TeSimPair of 
      ('a, 'b, 'c, 'd) _fterm *   (* Left term *)
      ('a, 'b, 'c, 'd) _fterm     (* Right term *)
  (* <t, t'> *)
  | TeAltPair of 
      ('a, 'b, 'c, 'd) _fterm *   (* Left term *)
      ('a, 'b, 'c, 'd) _fterm     (* Right term *)
  (* t | t' *)
  | TeUnionLeft of 
      ('a, 'b, 'c, 'd) _fterm *   (* Term *)
      ftype                       (* Injected type *)
  | TeUnionRight of 
      ftype *                     (* Injected type *)
      ('a, 'b, 'c, 'd) _fterm     (* Term *) 
  (* t! *)
  | TeBang of 
      ('a, 'b, 'c, 'd) _fterm *
      'a                      (* Type of bang *)
  (* refute with t *)
  | TeZero of 
      ('a, 'b, 'c, 'd) _fterm *   (* Proof of 0 *)
      'a                      (* Refuted type *)
  (*
   *  APPLICATIONS
   *)
  (* (x : A) -o t *)
  | TeLinAbs of 
      atom *                    (* Argument name *)
      ftype *                   (* Argument type *)
      ('a, 'b, 'c, 'd) _fterm   (* Body *)
  (* t t' *)
  | TeApp of
      ('a, 'b, 'c, 'd) _fterm *   (* Left arg *)
      ('a, 'b, 'c, 'd) _fterm *   (* Right arg *)
      'b                          (* Metadata (domain&codomain) *)
  (* forall [A] -> t *)
  | TeTyAbs of
      atom *                      (* Type variable name *)
      ('a, 'b, 'c, 'd) _fterm     (* Body *)
  (* t [A] *)
  | TeTyApp of
      ('a, 'b, 'c, 'd) _fterm *    (* Body *)
      ftype *                      (* Applied type *)
      'd                           (* Metadata (abstraction ctx) *)
  (* give x = t in t *)
  | TeGive of 
      atom *                  (* Binding name *)
      ('a, 'b, 'c, 'd) _fterm *   (* Binding value *)
      ('a, 'b, 'c, 'd) _fterm     (* Body *)
  (* 
   *  DESTRUCTORS
   *)
  (* match t return T with (p => M) *)
  | TeMatch of 
      ('a, 'b, 'c, 'd) _fterm *       (* Scrutinee *)
      ftype option *              (* Return type, if given *)
      ('a, 'b, 'c, 'd) _clause list *  (* Clauses *)
      'c                           (* Metadata (return type) *)
  (*
   *  ANNOTATIONS
   *)
  (* x : A *)
  | TeTyAnnot of 
      ('a, 'b, 'c, 'd) _fterm *  (* Term *)
      ftype                  (* Type *)   
  (* t *)
  | TeLoc of 
      location *             (* Position of term *)
      ('a, 'b, 'c, 'd) _fterm    (* Term *)


(* Clauses *)
and ('a, 'b, 'c, 'd) _clause = 
  (* p => M *)
  | Clause of 
      pattern *                    (* Pattern *)
      ('a, 'b, 'c, 'd) _fterm          (* Body *)

(* Pattern *)
and pattern = 
  (* * *)
  | PatOne
  (* _ *)
  | PatWildcard
  (* x *)
  | PatVar of 
      atom        (* Name of binding *)
  (* Data *)
  | PatData of 
      atom *        (* Data constructor *)
      atom list *   (* Type variables*)
      pattern list  (* Subpatterns *)
  (* x! *)
  | PatBang of 
      pattern     (* Subpattern *) 
  (* p, p *)
  | PatSimPair of
      pattern *   (* Left side of pair *) 
      pattern     (* Right side of pair *)
  (* <-,p,-> *)
  | PatAltPair of 
      int *       (* Amount of left skipped fields *) 
      pattern *   (* Pattern of matched field *)
      int         (* Amount of right skipped fields*)
  (* p <: A + _ + A *)
  | PatUnion of 
      pattern *         (* Subpattern *)
      (ftype list *     (* Types appearing left of hole *)
       ftype option)    (* Type appearing right of hole (if any) *)
  (* p | p *)
  | PatOr of 
      pattern *   (* Left pattern *)
      pattern     (* Right pattern *)
  (* p *)
  | PatLoc of 
      location *  (* Location *)
      pattern     (* Pattern *)
  (* p : A *)
  | PatTyAnnot of 
      pattern *   (* Subpattern *)
      ftype       (* Type annotation *)

and constant =
  (* Mul unit *)
  | TeOne 
  (* Add unit *)
  | TeTop

and integer = Int64.t

[@@deriving show, map, fold]

(* Declarations *)
type ('a, 'b, 'c, 'd, 'e, 'f, 'g) _declaration =
  (* Type declaration *)
  | DeclType of 
      type_decl *    (* Intended declaration *)
      'e *           (* Metadata (validated type declaration )*)
      'f             (* Metadata (new kinds table)*)
  (* Term declaration *)
  | DeclTerm of 
      ('a, 'b, 'c, 'd) _term_decl     (* Intended declaration *)
  (* Toplevel term *)
  | DeclTop of 
      ('a, 'b, 'c, 'd) _fterm *         (* Intended declaration *)
      'g                                (* Metadata (toplevel term's type) *)

and ('a, 'b, 'c, 'd) _term_decl = 
  (* Linear term decl *)
  | DteLinear of pattern * ('a, 'b, 'c, 'd) _fterm

and type_decl = 
  (* GADT decl *)
  | DtyGADT of atom * atom list * (atom * ftype) list

(* Program *)
and ('a, 'b, 'c, 'd, 'e, 'f, 'g) _program = 
  | Program of 
    ('a, 'b, 'c, 'd, 'e, 'f, 'g) _declaration list *  (* The declarations *)  
    'f                                                (* The kinds table *)
[@@deriving show, map]

(* Shorthands *)
(*
 *  PRE PETRIFICATION
 *)
type pre_fterm = 
  (type_metadata runtime,
   application_metadata runtime,
   type_metadata runtime,
   type_application_metadata runtime) _fterm
  
and pre_clause =
  (type_metadata runtime,
   application_metadata runtime,
   type_metadata runtime,
   type_application_metadata runtime) _clause

and pre_declaration = 
  (type_metadata runtime,
  application_metadata runtime,
  type_metadata runtime,
  type_application_metadata runtime,
  (type_ctor [@opaque]) runtime,
  (Kinds.env [@opaque]) runtime,
  type_metadata runtime
  ) _declaration

and pre_term_decl = 
  (type_metadata runtime,
  application_metadata runtime,
  type_metadata runtime,
  type_application_metadata runtime) _term_decl

and pre_program = 
  (type_metadata runtime,
   application_metadata runtime,
   type_metadata runtime,
   type_application_metadata runtime,
   (type_ctor [@opaque]) runtime,
   (Kinds.env [@opaque]) runtime,
   type_metadata runtime) _program

[@@deriving show, fold]

(*
 *  POST PETRIFICATION
 *)
type fterm = 
  (type_metadata,
   application_metadata,
   type_metadata,
   type_application_metadata) _fterm
  
and clause =
  (type_metadata,
   application_metadata,
   type_metadata,
   type_application_metadata) _clause

and declaration = 
  (type_metadata,
  application_metadata,
  type_metadata,
  type_application_metadata,
  (type_ctor [@opaque]),
  (Kinds.env [@opaque]),
  type_metadata) _declaration
 
and term_decl = 
  (type_metadata,
  application_metadata,
  type_metadata,
  type_application_metadata) _term_decl

and program = 
  (type_metadata,
   application_metadata,
   type_metadata,
   type_application_metadata,
   (type_ctor [@opaque]),
   (Kinds.env [@opaque]),
   type_metadata) _program

[@@deriving show, fold]

(* Helpers *)

(* Petrifaction amounts to forcing every metadata throughout. *) 
let petrify_fterm (term: pre_fterm): fterm =
  map__fterm force force force force term

let petrify_decl (decl: pre_declaration): declaration = 
  map__declaration force force force force force force force decl

let petrify (Program (decl, kinds) : pre_program) =
  Program ((List.map petrify_decl decl), (force kinds))