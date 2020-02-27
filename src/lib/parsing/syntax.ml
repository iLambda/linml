open Utils.Error
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

(* ------------------------------------------------------------------------- *)

(* AST. *)
type ftype = 
  (* Constants *)
  | SynTyOne | SynTyTop | SynTyZero
  (* A *)
  | SynTyVarOrTyCon of (string * (Lexing.position[@opaque]) * (Lexing.position[@opaque])) * ftype list
  (* A + B *)
  | SynTyPlus of ftype * ftype
  (* A * B *)
  | SynTyTensor of ftype * ftype
  (* A & B *)
  | SynTyWith of ftype * ftype
  (* A -o B *)
  | SynTyLollipop of ftype * ftype
  (* A -> B *)
  | SynTyArrow of ftype * ftype
  (* A! *)
  | SynTyBang of ftype

and constant = 
  (* Mul unit *)
  | SynTeOne
  (* Add unit *)
  | SynTeTop

and integer = Int64.t

and fterm =
  (*
   *  LEAFS
   *)
  (* Constant *)
  | SynTeConst of constant 
  (* x *)
  | SynTeVar of identifier
  (* 
   *  CONSTRUCTS
   *)
  (* t, t' *)
  | SynTeSimPair of fterm * fterm
  (* <t, t'> *)
  | SynTeAltPair of fterm * fterm
  (* t | t' *)
  | SynTeUnionLeft of fterm * ftype
  | SynTeUnionRight of ftype * fterm
  (* t! *)
  | SynTeBang of fterm
  (*
   *  APPLICATIONS
   *)
  (* (x : A) -o t *)
  | SynTeLinAbs of identifier * ftype * fterm
  (* t t' *)
  | SynTeApp of fterm * fterm
  (* give x : A = t *)
  | SynTeGive of identifier * fterm * fterm
  (* 
   *  DESTRUCTORS
   *)
  (* match t return T with (p => M) *)
  | SynTeMatch of fterm * ftype option * clause list
  (*
   *  ANNOTATIONS
   *)
  (* x : A *)
  | SynTeTyAnnot of fterm * ftype
  (* t *)
  | SynTeLoc of location * fterm

and clause = 
  (* p => M *)
  | SynClause of pattern * fterm

and pattern = 
  (* * *)
  | SynPatOne
  (* x *)
  | SynPatVar of identifier
  (* x : A *)
  | SynPatTyAnnot of pattern * ftype
  (* x! *)
  | SynPatBang of pattern 
  (* p, p *)
  | SynPatSimPair of pattern * pattern
  (* <-,p,-> *)
  | SynPatAltPair of int * pattern * int
  (* p <: A + _ + A *)
  | SynPatUnion of pattern * (ftype list * ftype option)
  (* p | p *)
  | SynPatOr of pattern * pattern
  (* p *)
  | SynPatLoc of location * pattern     (* Pattern *)

and program =
  SynProg of fterm
  
[@@deriving show { with_path = false }]  
