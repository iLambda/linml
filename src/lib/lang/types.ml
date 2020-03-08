open Utils
open Utils.Atom
open Utils.Identifier

(* Needed, else ppx_compare complains *)
let compare_int = Stdlib.compare

(* Type contexts *)
type ftype_context = {
  (* The identifier used to carve a hole *)
  hint: identifier [@compare.ignore];
  (* The context *)
  body: ftype;
} 
(* Types of terms *)
and ftype =   
  | TyOne | TyTop | TyZero
  | TyBoundVar of int
  | TyFreeVar of atom
  | TyCon of atom * ftype list
  | TyPlus of ftype * ftype
  | TyTensor of ftype * ftype
  | TyWith of ftype * ftype
  | TyLollipop of ftype * ftype
  | TyArrow of ftype * ftype
  | TyBang of ftype
  | TyForall of ftype_context

[@@deriving compare, show { with_path = false }]  

(* ------------------------------------------------------------------------------- *)
(* Type contexts *)

(** [abstract a ty] replaces every free occurrence of the type variable
    [a] in the type [ty] with a hole, producing a type context. *)

let rec abstract a i ty =   
  match ty with
    (* This is the right atom ; abstract *)
    | TyFreeVar b when Atom.equal a b -> TyBoundVar i
    (* Base cases *)
    | TyOne | TyTop | TyZero 
    | TyFreeVar _ | TyBoundVar _ -> ty
    (* Recursive *)
    | TyPlus (t1, t2) -> TyPlus (abstract a i t1, abstract a i t2)
    | TyTensor (t1, t2) -> TyTensor (abstract a i t1, abstract a i t2)
    | TyWith (t1, t2) -> TyWith (abstract a i t1, abstract a i t2)
    | TyLollipop (t1, t2) -> TyLollipop (abstract a i t1, abstract a i t2)
    | TyArrow (t1, t2) -> TyArrow (abstract a i t1, abstract a i t2)
    | TyBang (t) -> TyBang (abstract a i t)
    (* Forall ! Increment bound variable *)
    | TyForall { hint; body } -> TyForall { hint; body=abstract a (i+1) body }

let abstract a ty = 
  let hint = Atom.identifier a in 
  let body = abstract a 0 ty in
  { hint; body }

(** [fill c ty] fills the type context [c] with the type [ty], producing a
    type. *)

let rec fill i ty c =   
  match c with
    (* This is the right atom ; abstract *)
    | TyBoundVar j when i = j -> ty
    (* Base cases *)
    | TyOne | TyTop | TyZero 
    | TyFreeVar _ | TyBoundVar _ -> c
    (* Recursive *)
    | TyPlus (t1, t2) -> TyPlus (fill i ty t1, fill i ty t2)
    | TyTensor (t1, t2) -> TyTensor (fill i ty t1, fill i ty t2)
    | TyWith (t1, t2) -> TyWith (fill i ty t1, fill i ty t2)
    | TyLollipop (t1, t2) -> TyLollipop (fill i ty t1, fill i ty t2)
    | TyArrow (t1, t2) -> TyArrow (fill i ty t1, fill i ty t2)
    | TyBang t -> TyBang (fill i ty t)
    (* Forall ! Increment bound variable *)
    | TyForall { hint; body } -> TyForall { hint; body=fill (i+1) ty body }

let fill { body; _ } ty = 
  fill 0 ty body
    
(** [hint c] suggests an identifier to represent the hole in the type
    context [c]. *)

let hint { hint; _ } =
  hint
    
(* ------------------------------------------------------------------------------- *)
(* Types *)


(** [count_foralls ty] indicates how many universal quantifiers appear at the
   root of the type [ty] *)

let rec count_foralls accu = function
  | TyForall { body; _ } -> count_foralls (accu+1) body
  | _ -> accu

let count_foralls ty = count_foralls 0 ty
    
(* [multiplicity t] returns the multiplicity of the type *)
let multiplicity _ = 1

(* [multiply t m] gives multiplicity [m] to type [t] *)
let multiply t = function 
  (* Check multiplicity *)
  | n when n <= 0 -> failwith "Multiplicity must be greater than zero"
  | 1 -> t 
  | _ -> failwith "Unsupported for now" 

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
    (* Type constructors *)
    | TyCon (x1, tys1), TyCon (x2, tys2) -> 
        Atom.equal x1 x2
        && List.compare_lengths tys1 tys2 = 0 
        && List.for_all2 equal tys1 tys2
    (* Free type variables  *)
    | TyBoundVar i, TyBoundVar j -> i = j
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
    | TyForall { body=body1; _ }, TyForall { body=body2; _ } ->
        equal body1 body2
    (* Else *)
    | _ -> false
    