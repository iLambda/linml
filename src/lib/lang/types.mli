open Utils.Atom
open Utils.Identifier

(* Type contexts *)
type ftype_context

(* Types of terms *)
type ftype =   
  (* Constants *)
  | TyOne | TyTop | TyZero
  (* i ; This type constructor is not to be used outside of this module *)
  | TyBoundVar of int
  (* A *)
  | TyFreeVar of atom
  (* Type constructor *)
  | TyCon of atom * ftype list
  (* A + B *)
  | TyPlus of ftype * ftype
  (* A * B *)
  | TyTensor of ftype * ftype
  (* A & B *)
  | TyWith of ftype * ftype
  (* A -o B *)
  | TyLollipop of ftype * ftype
  (* A -> B *)
  | TyArrow of ftype * ftype
  (* A! *)
  | TyBang of ftype
  (* forall a. A *)
  | TyForall of ftype_context

(* ------------------------------------------------------------------------------- *)
(* Type contexts *)

(** [abstract a ty] replaces every free occurrence of the type variable
    [a] in the type [ty] with a hole, producing a type context. *)
val abstract: atom -> ftype -> ftype_context

(** [fill c ty] fills the type context [c] with the type [ty], producing a
   type. *)
val fill: ftype_context -> ftype -> ftype

(** [hint c] suggests an identifier to represent the hole in the type
   context [c]. *)
val hint: ftype_context -> identifier

(* ------------------------------------------------------------------------------- *)
(* Types *)

(** [equal] tells whether two types are equal up to alpha-equivalence. *)
val equal: ftype -> ftype -> bool

(** [is_exponential t] returns true iff the type has an exponential modality *)
val is_exponential: ftype -> bool

(** [multiplicity t] returns the multiplicity of the type *)
val multiplicity: ftype -> int

(** [multiply t m] gives multiplicity [m] to type [t] *)
val multiply: ftype -> int -> ftype 

(** [count_foralls ty] indicates how many universal quantifiers appear at the
   root of the type [ty] *)
val count_foralls: ftype -> int

(** [arity t] returns the arity of the function type [t]. 
    If t is not a function type, it returns 0 *)
val arity: ftype -> int

(* ------------------------------------------------------------------------------- *)
(* Ppx deriving *)

val pp_ftype: Format.formatter -> ftype -> unit
val compare_ftype: ftype -> ftype -> int