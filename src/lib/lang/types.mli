open Utils.Atom

(* Types of terms *)
type ftype =   
  (* Constants *)
  | TyOne | TyTop | TyZero
  (* A *)
  | TyFreeVar of atom
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
  

(* [equal] tells whether two types are equal up to alpha-equivalence. *)
val equal: ftype -> ftype -> bool
(* [is_exponential t] returns true iff the type has an exponential modality *)
val is_exponential: ftype -> bool
(* [multiplicity t] returns the multiplicity of the type *)
val multiplicity: ftype -> int

val pp_ftype: Format.formatter -> ftype -> unit
val compare_ftype: ftype -> ftype -> int