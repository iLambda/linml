open Lang
open Lang.Types
open Printer
open ANSITerminal
open Utils.Atom

(* Errors thrown when an operation is invalid *)
exception Unbound of atom 
exception NoMore of atom 
exception Bound of atom * int
exception NoBindTop of atom

(* The type of environments *)
type env = envbinding AtomMap.t
and linenv = (ftype * int) AtomMap.t
and envbinding = 
  | BindLin of ftype * int 
  | BindExp of ftype

(* Sets of bindings *)
type binding = atom * ftype  
[@@deriving ord]

module Binding = struct
  type t = atom * ftype
  let compare = compare_binding
end
module Bindings =
  Set.Make (Binding)

(* The reason for the separation *)
type separation =
  | DifferentMult of int * int
  | DifferentType of ftype * ftype

(* ------------------------------------------------------------------------------- *)
(* Helpers *)


(* ------------------------------------------------------------------------------- *)
(* Constants *)

(* [empty] represents the empty environment *)
let empty : env = AtomMap.empty

(** [nothing] represents the linear empty environment *)
let nothing : linenv = AtomMap.empty

(* ------------------------------------------------------------------------------- *)
(* Environment manipulation *)

(** [consume env x] consumes a binding from the environment.
    Raises [Unbound] if [x] not present.
    Raises [NoMore x] if [x] is depleted.  *)
let consume env x : env * ftype = 
    (* Get binding *)
    match AtomMap.find_opt x env with 
      (* Unbound *)
      | None -> raise (Unbound x)
      (* Exponential binding *)
      | Some (BindExp ty) -> env, ty
      (* Alive linear binding. Get and decrease*) 
      | Some (BindLin(ty, m)) when m > 0 -> 
          (AtomMap.add x (BindLin (ty, m-1)) env), ty
      (* Dead linear binding. Error *)
      | Some (BindLin (_, 0)) -> raise (NoMore x) 
      (* Cannot be reached - negative multiplicity *)
      | Some (BindLin (_, _)) -> assert false

(** [bind env x ty] binds a to [x] the type [ty] in the environment.
    Raises [Bound (x, multiplicity)] if [x] was already bound.
    Raises [NoBindTop] if [ty] is top. *)
let bind env x ty : env = 
  (* If type is top, error *)
  if ty = TyTop then raise (NoBindTop x);
  (* Make binding  *)
  let binding = 
    if Types.is_exponential ty 
    then BindExp ty 
    else BindLin (ty, Types.multiplicity ty) 
  in
  (* Get binding *)
  match AtomMap.find_opt x env with 
    (* Not found, or dead linear binding, or exponential.
      Okay to change since nothing is erased/we weaken *)
    | None | Some (BindLin (_, 0)) | Some (BindExp _) -> 
        AtomMap.add x binding env 
    (* There was one alive linear binding, but we are not allowed to erase it *) 
    | Some (BindLin (_, m)) when m > 0 -> raise (Bound (x, m))
    (* Cannot be reached - negative multiplicity *)
    | Some (BindLin (_, _)) -> assert false 
  
(** [binds env b] binds all the bindings [x] in the environment.
    Raises [Bound (x, multiplicity)] if [x] was already bound.
    Raises [NoBindTop] if [ty] is top. *)
let binds env b = 
  Bindings.fold (fun (x, ty) env -> bind env x ty) b env

(* ------------------------------------------------------------------------------- *)
(* Environment composition *)

(** [linearize env] returns a linear environment, containing only the linear variables of [env] *)
let linearize env : linenv =
  AtomMap.merge 
    (fun _ _ b -> 
      match b with 
        (* Keep linear bindings *)
        | Some (BindLin (t, m)) -> Some (t, m)
        (* Discard *)
        | _ -> None)
    env 
    env 

(** [exponentials env] returns an environment, containing only the exponential bindings of [env] *)
let exponentials env : env = 
  AtomMap.merge 
    (fun _ _ b -> 
      match b with 
        (* Keep exp bindings *)
        | Some (BindExp _ as b) -> Some b
        (* Discard *)
        | _ -> None)
    env 
    env 

(** [compose lenv eenv] returns a new environment containing the linear bindings of [lenv]
 and the exponential bindins of [env] *)
let compose envl enve : env = 
  AtomMap.merge 
    (fun _ linb expb ->
      match linb, expb with 
        (* Select the linear bindings *)
        | Some (t, m), _ -> Some (BindLin(t, m))
        (* Select the exponential bindings *)
        | _, Some (BindExp _ as b) -> Some b
        (* Ignore *)
        | _ -> None)
    envl
    enve

(** [of_bindings b] returns a new environment containing the linear bindings of [b] *)
let of_bindings bindings : linenv = 
  (* Make env *)
  linearize (binds empty bindings)


(* ------------------------------------------------------------------------------- *)
(* Multiset operations *)

(** [is_void lenv] returns true iff the linear environment and the exponential env are empty *)
let is_void env : bool = 
  AtomMap.is_empty env

(** [is_empty env] returns true iff the linear environment is empty *)
let is_empty env : bool = 
  let empty_checker _ = function (_, 0) -> true | _ -> false in
  AtomMap.for_all empty_checker env

(** [has lenv x] checks if a variable already belongs to a linear environment *)
let has lenv x : bool = 
  match AtomMap.find_opt x lenv with 
    | None -> false 
    | Some (_, 0) -> false 
    | _ -> true
  
(** [multiplicity lenv x] checks the multiplicity of [x] in [lenv] *)
let multiplicity lenv x = 
  let _, m = AtomMap.find x lenv in m

(** [equal env env'] checks if two environments are equal *)
let equal env env' : bool = 
  (* Compare *)
  AtomMap.for_all2 
    (fun _ b1 b2 ->
      match b1, b2 with
      (* Both dead/not found *)
      | None, None
      | None, Some (_, 0)
      | Some (_, 0), None
      | Some (_, 0), Some (_, 0) -> true 
      (* Both alive, of same value *)
      | Some (ty, m), Some (ty', m') when Types.equal ty ty' && m = m' -> true
      (* Discrepancy *)
      | _ -> false)
    env
    env'

(** [subset lenv lenv'] returns true iff [env'] is a subset of [env] *)
let subset env env'  =
  (* Compare *)
  AtomMap.for_all2 
    (fun _ b1 b2 ->
      match b1, b2 with
      (* Both dead/not found *)
      | None, None
      | None, Some (_, 0)
      | Some (_, 0), None
      | Some (_, 0), Some (_, 0) -> true 
      (* Alive in larger, not found in smaller *)
      | Some _, None -> true
      (* Both alive, but right value <= left value *)
      | Some (ty, m), Some (ty', m') when Types.equal ty ty' && m >= m' -> true
      (* Discrepancy *)
      | _ -> false)
    env
    env'

(** [difference lenv lenv'] returns the linear variables that are in [env] 
    but not in [env']*)
let difference env env' : linenv = 
  AtomMap.merge 
    (fun _ b1 b2 -> 
      match b1, b2 with
        (* Dead/not found on both sides *)
        | None, None
        | None, Some (_, 0)
        | Some (_, 0), None
        | Some (_, 0), Some (_, 0) -> None
        (* Greater linear binding in [env] *)
        | Some b, None -> Some b
        | Some (t, m), Some (t', m') when Types.equal t t' && m >= m' -> Some (t, m - m')
        (* Else, not a subset *)
        | _ -> failwith "Not a subset")
    env
    env'

(** [inter lenv lenv'] returns the intersection of the environments*)
let inter env env' : linenv = 
  AtomMap.merge 
    (fun _ b1 b2 -> 
      match b1, b2 with
        (* Dead/not found on both sides *)
        | None, None
        (* Binding in only one env *)
        | Some _, None  
        | None, Some _ -> None 
        (* Binding found in both ; select minimum *)
        | Some (t, m), Some (t', m') when Types.equal t t' -> Some (t, min m m')
        (* Else, not a subset *)
        | _ -> failwith "Not compatible")
    env
    env'

(** [split env x] returns two environments.
    The second one containing just the binding for [x] (or is empty if [x] is not in [env]),
    the other containing the rest of [env] *)
let split env x =
  (* Check *)
  match AtomMap.find_opt x env with 
    | None -> env, nothing
    | Some b -> AtomMap.remove x env, AtomMap.singleton x b
  
(** [merge env env'] merges two environments.
    Raises [Failure] if they are not disjoint *)
let merge env env' = 
  AtomMap.union
    (fun _ b b' -> 
      match b, b' with 
        (* Check if they have the same type *)
        (ty, m), (ty', m') when Types.equal ty ty' -> Some (ty, m + m')
        (* Not disjoint *)
        | _ -> failwith "Incompatible typing.")
    env
    env'
      
(** [purge env x] removes x from the environment altogether *)
let purge env x = 
  AtomMap.remove x env

(** [purges env b] removes all the bindings [b] from the environment altogether *)
let purges env b = 
  Bindings.fold (fun (x, _) env -> AtomMap.remove x env) b env

(* ------------------------------------------------------------------------------- *)
(* Selection *)

(* [pick env] picks a linear variable from the environment *)
let pick env : atom * int =
  (* Find binding and get its multiplicity *)
  let nonempty_checker _ = function (_, 0) -> false | _ -> true in
  let x, (_, m) = AtomMap.pick nonempty_checker env in
  x, m

(** [separate env env'] picks a binding that is either in [env] and not in [env'], or 
in [env'] but not in [env] *)
let separate lenv lenv' : atom * separation = 
  (* Pick the sepration *)
  try AtomMap.pickmap2 
        (fun _ b b' -> 
          match b, b' with
            (* Dead/not found *)
            | None, None | None, Some (_, 0)
            | Some (_, 0), None | Some (_, 0), Some (_, 0) -> None
            (* A binding was found, and wasn't in one of the envs *)
            | None, Some (_, m)
            | Some (_, 0), Some (_, m) -> Some (DifferentMult (0, m))
            | Some (_, m), None
            | Some (_, m), Some (_, 0) -> Some (DifferentMult (m, 0))
            (* A binding was found in both envs but wasn't of the same type *)
            | Some (ty, _), Some (ty', _) when not (Types.equal ty ty') ->
                Some (DifferentType (ty, ty'))
            (* A binding was found in both envs but wasn't of the same multiplicity *)
            | Some (_, m), Some (_, m') when m <> m' ->
                Some (DifferentMult (m, m'))
            (* The same *)
            | _ -> None )
        lenv
        lenv'
  with 
    Not_found -> failwith "Environments are equal"
  

(* ------------------------------------------------------------------------------- *)
(* Print helpers *)

(** [print env] returns a string representing the linear environment *)

let descriptor_ty color xenv x ty = 
  (* Check if color needed *)
  let sprintf = 
    if color 
    then ANSITerminal.sprintf [Bold; Reset; Bold]
    else Printf.sprintf
  in
  (* Print *)
  sprintf 
    "%s %s : %s"
    "val"
    (print_atom xenv x)
    (print_type xenv ty)


let descriptor color xenv x = function 
  | BindLin (_, 0) -> None 
  | BindLin (ty, m) -> Some (descriptor_ty color xenv x (Types.multiply ty m))
  | BindExp ty -> Some (descriptor_ty color xenv x ty)

let print ?(color=false) env xenv = 
  let bindings = AtomMap.bindings env in 
  let descriptors = 
    List.filter_map
      (fun (x, b) -> descriptor color xenv x b)
      bindings
    in 
  String.concat "\n" descriptors

(** [print_top xenv env] returns a string representing the toplevel term of given type *)
let print_top ?(color=false) ty xenv =
  let x = (Utils.Atom.fresh (Utils.Identifier.mk "-" Lang.Sort.term_sort)) in 
  let xenv = Utils.Export.bind xenv x in
  descriptor_ty color xenv x ty