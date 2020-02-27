open Utils
open Utils.Atom

(* Types of terms *)
type ftype =   
  | TyOne | TyTop | TyZero
  | TyFreeVar of atom
  | TyPlus of ftype * ftype
  | TyTensor of ftype * ftype
  | TyWith of ftype * ftype
  | TyLollipop of ftype * ftype
  | TyArrow of ftype * ftype
  | TyBang of ftype

[@@deriving ord, show { with_path = false }]  

(* [multiplicity t] returns the multiplicity of the type *)
let multiplicity _ = 1

(* [is_exponential t] returns true iff the type has an exponential modality *)
let is_exponential = function 
  (* These types are exponential *)
  | TyOne -> true
  | TyBang _-> true
  (* | TyTensor (ty1, ty2) -> is_exponential ty1 && is_exponential ty2 *)
  (* All the other types are not *)
  | _ -> false

(* [equal] tells whether two types are equal up to alpha-equivalence. *)
let rec equal ty1 ty2 =
  match ty1, ty2 with
    (* Constants *)
    | TyOne, TyOne -> true
    | TyTop, TyTop -> true
    | TyZero, TyZero -> true
    (* Free type variables  *)
    | TyFreeVar a1, TyFreeVar a2 -> Atom.equal a1 a2
    (* Bangs *)
    | TyBang (TyBang ty), TyBang ty' 
    | TyBang ty, TyBang (TyBang ty') 
    | TyBang ty, TyBang ty' -> equal ty ty'
    (* Recursive cases *)
    | TyPlus (ty1, ty1'), TyPlus (ty2, ty2') 
    | TyTensor (ty1, ty1'), TyTensor (ty2, ty2') 
    | TyWith (ty1, ty1'), TyWith (ty2, ty2') 
    | TyLollipop (ty1, ty1'), TyLollipop (ty2, ty2') 
    | TyArrow (ty1, ty1'), TyArrow (ty2, ty2') ->
        equal ty1 ty2 && equal ty1' ty2'
    (* Else *)
    | _ -> false
    