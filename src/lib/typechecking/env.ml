open Lang
open Lang.Types
open Utils.Atom

(* Errors thrown when an operation is invalid *)
exception Unbound of atom 
exception NoMore of atom 
exception Bound of atom * int
exception Incompatible

(* The type of environment *)
type env = binding AtomMap.t
and binding = 
  | BindLin of ftype * int 
  | BindExp of ftype

(* The type of digests *)
type digest = (ftype * int) AtomMap.t

(* [empty] represents the empty environment *)
let empty : env = AtomMap.empty

(* [is_empty env] returns true iff the environment only has exponential bindings *)
let is_empty env : bool = 
  let empty_checker _ = function 
    BindExp _ | BindLin (_, 0) -> true
    | _ -> false in
  AtomMap.for_all empty_checker env
  
(* [pick env] picks a non-exponential variable from the environment *)
let pick env : atom * int =
  let lin_checker x = 
    let b = AtomMap.find x env in 
    match b with BindLin (_, m) when m > 0 -> true | _ -> false 
  in
  (* Find binding and get its multiplicity *)
  match AtomMap.find_first lin_checker env with 
    | x, BindLin (_, m) -> x, m
    | _ -> assert false

(* Bind a variable in the environment *)
let bind env x ty : env = 
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
  
(* Retrieve a binding from the environment *)
let retrieve env x : env * ftype = 
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

(* Merge two environments if they are compatible *)
let union env1 env2 = 
  (* Conflict resolver *)
  let conflict _ b1 b2 = 
    match b1, b2 with 
      (* Two lin bindings of the same type. Just add multiplicities *)
      | BindLin (t1, m1), BindLin (t2, m2) when Types.equal t1 t2 ->
          Some (BindLin (t1, m1 + m2))
      (* Two exp bindings of the same type. Keep it *)
      | BindExp t1, BindExp t2 when Types.equal t1 t2 -> 
          Some (BindExp (t1))
      (* Else, incompatible *)
      | _ -> raise Incompatible
  in 
  AtomMap.union conflict env1 env2 

(** [equal env env'] checks if two environments are equal *)
let equal env env' : bool = 
  (* Compare *)
  let equal_binding_checker env x b = 
    match b, AtomMap.find_opt x env with 
      (* Dead, or not found. Okay *)
      | BindLin (_, 0), None
      | BindLin (_, 0), Some (BindLin (_, 0)) -> true 
      (* Alive *)
      | BindLin (ty, m), Some (BindLin (ty', m'))
        when Types.equal ty ty' && m = m' -> true
      (* Exponentials *)
      | BindExp ty, Some (BindExp ty') when Types.equal ty ty' -> true
      (* Else, no match. Not ok *)
      | _ -> false 
  in
  (* Check that both are free of bindings in the other *)
  AtomMap.for_all (equal_binding_checker env) env' &&
  AtomMap.for_all (equal_binding_checker env') env

(** [disjoint env env'] returns true iff the environments are disjoint *)
let disjoint env env' : bool = 
  let no_binding_checker env x _ = 
    match AtomMap.find_opt x env with 
      (* Dead, or not found. Okay *)
      | None | Some (BindLin (_, 0)) -> true  
      (* Else, alive or exponential. Not ok *)
      | _ -> false 
  in
  (* Check that both are free of bindings in the other *)
  AtomMap.for_all (no_binding_checker env) env' &&
  AtomMap.for_all (no_binding_checker env') env

(** [inter env env'] returns the intersection of the environments *)
let inter env env' : env = 
  AtomMap.merge
    (fun _ b b' ->  
      match b, b' with 
        (* No bindings *)
        | None, None -> None
        (* Trace of an old binding ; say it's been erased *)
        | Some (BindLin (ty, 0)), None
        | None, Some (BindLin (ty, 0)) 
        | Some (BindLin (ty, 0)), Some (BindLin (_, 0)) -> 
            Some (BindLin (ty, 0))
        (* Two linear bindings. Check type equal, and return minimum *)
        | Some(BindLin (ty, m)), Some(BindLin (ty', m')) when Types.equal ty ty' -> 
            Some (BindLin (ty, min m m'))
        (* Exponential bindings. Check type and return *)
        | Some(BindExp t), Some(BindExp t') when Types.equal t t' ->  
            Some (BindExp t)
        (* Incompatible *)
        | _ -> raise Incompatible)
    env
    env'

(** [consumed env env'] returns the linear variables that were consumed *)
let consumed env env' : env = 
  AtomMap.fold 
    (fun x binding digest -> 
      match binding, AtomMap.find_opt x env' with
        (* Dead linear binding vs absence. No difference *)
        | BindLin (_, 0), None -> digest
        (* Alive linear binding vs absence. Count *)
        | BindLin (t, m), None when m > 0 -> AtomMap.add x (BindLin (t, m)) digest
        (* Linear binding, count *)
        | BindLin (t, m), Some (BindLin (t', m')) when Types.equal t t' -> 
            if m > m' 
              then AtomMap.add x (BindLin(t, m - m')) digest
              else digest
        (* Exponential binding, ignore *)
        | BindExp t, Some (BindExp t') when Types.equal t t' -> 
            digest
        (* Else, incompatible *)
        | _ -> raise Incompatible)
    env
    empty

(** [compose envl enve] returns a new environment containing the linear bindings of [env]
 and the exponential bindins of [enve] *)
let compose envl enve : env = 
  AtomMap.merge 
    (fun _ linb expb ->
      match linb, expb with 
        (* Select the linear bindings *)
        | Some (BindLin _ as b), _ -> Some b
        (* Select the exponential bindings *)
        | _, Some (BindExp _ as b) -> Some b
        (* Ignore *)
        | _ -> None)
    envl
    enve
