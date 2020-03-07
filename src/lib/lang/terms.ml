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

[@@deriving show]

(* Terms *)
type ('a, 'b, 'c) _fterm = 
  (*
   *  LEAFS
   *)
  (* Constant *)
  | TeConst of constant 
  (* x *)
  | TeVar of 
      atom *  (* Variable name *)
      'a      (* Metadata (type of variable) *)
  (* 
   *  CONSTRUCTS
   *)
  (* t, t' *)
  | TeSimPair of 
      ('a, 'b, 'c) _fterm *   (* Left term *)
      ('a, 'b, 'c) _fterm     (* Right term *)
  (* <t, t'> *)
  | TeAltPair of 
      ('a, 'b, 'c) _fterm *   (* Left term *)
      ('a, 'b, 'c) _fterm     (* Right term *)
  (* t | t' *)
  | TeUnionLeft of 
      ('a, 'b, 'c) _fterm *   (* Term *)
      ftype                   (* Injected type *)
  | TeUnionRight of 
      ftype *                 (* Injected type *)
      ('a, 'b, 'c) _fterm     (* Term *) 
  (* t! *)
  | TeBang of 
      ('a, 'b, 'c) _fterm *   (* Term *)
      'a                      (* Type of bang *)
  (* refute with t *)
  | TeZero of 
      ('a, 'b, 'c) _fterm *   (* Proof of 0 *)
      'a                      (* Refuted type *)
  (*
   *  APPLICATIONS
   *)
  (* (x : A) -o t *)
  | TeLinAbs of 
      atom *                  (* Argument name *)
      ftype *                 (* Argument type *)
      ('a, 'b, 'c) _fterm     (* Body *)
  (* t t' *)
  | TeApp of
      ('a, 'b, 'c) _fterm *   (* Left arg *)
      ('a, 'b, 'c) _fterm *   (* Right arg *)
      'b                      (* Metadata (domain&codomain) *)
  (* give x = t in t *)
  | TeGive of 
      atom *                  (* Binding name *)
      ('a, 'b, 'c) _fterm *   (* Binding value *)
      ('a, 'b, 'c) _fterm     (* Body *)
  (* 
   *  DESTRUCTORS
   *)
  (* match t return T with (p => M) *)
  | TeMatch of 
      ('a, 'b, 'c) _fterm *       (* Scrutinee *)
      ftype option *              (* Return type, if given *)
      ('a, 'b, 'c) _clause list *  (* Clauses *)
      'c                           (* Metadata (return type) *)
  (*
   *  ANNOTATIONS
   *)
  (* x : A *)
  | TeTyAnnot of 
      ('a, 'b, 'c) _fterm *  (* Term *)
      ftype                  (* Type *)   
  (* t *)
  | TeLoc of 
      location *             (* Position of term *)
      ('a, 'b, 'c) _fterm    (* Term *)


(* Clauses *)
and ('a, 'b, 'c) _clause = 
  (* p => M *)
  | Clause of 
      pattern *                    (* Pattern *)
      ('a, 'b, 'c) _fterm          (* Body *)

(* Pattern *)
and pattern = 
  (* * *)
  | PatOne
  (* _ *)
  | PatWildcard
  (* x *)
  | PatVar of 
      atom        (* Name of binding *)
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

(* Program *)
type ('a, 'b, 'c) _program = 
  | Program of ('a, 'b, 'c) _fterm
  
[@@deriving show]

(* Shorthands *)
(*
 *  PRE PETRIFICATION
 *)
type pre_fterm = 
  (type_metadata runtime,
   application_metadata runtime,
   type_metadata runtime) _fterm
  
and pre_clause =
  (type_metadata runtime,
   application_metadata runtime,
   type_metadata runtime) _clause

and pre_program = 
  (type_metadata runtime,
   application_metadata runtime,
   type_metadata runtime) _program

[@@deriving show, fold]

(*
 *  POST PETRIFICATION
 *)
type fterm = 
  (type_metadata,
   application_metadata,
   type_metadata) _fterm
  
and clause =
  (type_metadata,
   application_metadata,
   type_metadata) _clause

and program = 
  (type_metadata,
   application_metadata,
   type_metadata) _program

[@@deriving show, fold]

(* Helpers *)

(* Petrifaction amounts to forcing every metadata throughout. *) 
let petrify_fterm (term: pre_fterm): fterm =
  map__fterm force force force term

let petrify (Program (term)) =
  Program (petrify_fterm term)
  