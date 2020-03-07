open Lang
open Lang.Sort
open Lang.Types
open Lang.Terms
open Syntax
open Utils
open Utils.Atom
open Utils.Error
open Utils.Identifier

type atom =
    Atom.atom

type env =
    Import.env

(* Term variables, type variables, type constructors, and data constructors are
   explicitly bound. *)
let bind env id : atom * env =
  let env = Import.bind env id in
  Import.resolve env id, env

let free _ _ = () (*Import.free*)

(* [itype] converts a type from external syntax into internal syntax. *)
let ityvar env a =
  TyFreeVar (Import.resolve env a)

(* let ityvars env =
  List.map (ityvar env) *)

let rec itype tctable env : Syntax.ftype -> Types.ftype = function
  (* Constants *)
  | SynTyOne -> TyOne
  | SynTyTop -> TyTop
  | SynTyZero -> TyZero
  (* Recusrive *)
  | SynTyPlus (ty, ty') -> TyPlus (itype tctable env ty, itype tctable env ty')
  | SynTyTensor (ty, ty') -> TyTensor (itype tctable env ty, itype tctable env ty')
  | SynTyWith (ty, ty') -> TyWith (itype tctable env ty, itype tctable env ty')
  | SynTyLollipop (ty, ty') -> TyLollipop (itype tctable env ty, itype tctable env ty')
  | SynTyArrow (ty, ty') -> TyArrow (itype tctable env ty, itype tctable env ty')
  | SynTyBang (ty) -> TyBang (itype tctable env ty)
  (* Type free variables / type constructors *)
  | SynTyVarOrTyCon (id, []) when Import.resolves env type_sort id ->
  (* | SynTyVarOrTyCon (id, []) when Import.resolves env type_sort id -> *)
      (* if there are no type arguments and if [id] resolves as a type variable [a],
         then consider it is a type variable *)
      let a = Identifier.mak type_sort id in ityvar env a
  | SynTyVarOrTyCon _ -> failwith "No type constructors yet"

and itypeo tctable env = function 
  | None -> None 
  | Some ty -> Some (itype tctable env ty)

and itypes tctable env tys =
  List.map (itype tctable env) tys

(* [iterm] converts a term from external syntax into internal syntax. *)
let rec iterm tctable env = function
  (*
   *  LEAFS
   *)
  (* Constants *)
  | SynTeConst c -> TeConst (iconstant c)
  (* x *)
  | SynTeVar id -> TeVar (Import.resolve env id, reset ())
  (* 
   *  CONSTRUCTS
   *)
  (* t, t' *)
  | SynTeSimPair (t, t') -> TeSimPair (iterm tctable env t, iterm tctable env t')
  (* <t, t'> *)
  | SynTeAltPair (t, t') -> TeAltPair (iterm tctable env t, iterm tctable env t')
  (* t | t' *)
  | SynTeUnionLeft (t, ty) -> TeUnionLeft (iterm tctable env t, itype tctable env ty)
  | SynTeUnionRight (ty, t) -> TeUnionRight (itype tctable env ty, iterm tctable env t)
  (* t! *)
  | SynTeBang t -> TeBang (iterm tctable env t, reset ())
  (* refute T with t *)
  | SynTeZero t -> TeZero (iterm tctable env t, reset ())
  (*
   *  APPLICATIONS
   *)
  (* (x : A) -o t *)
  | SynTeLinAbs (x, ty, t) ->
    (* Shadowing allowed in applications, since all free variables 
       we access have to be banged, and hence can be shadowed *)
    let x, env = bind env x in
    TeLinAbs (x, itype tctable env ty, iterm tctable env t)
  (* t t' *)
  | SynTeApp (t, t') -> TeApp (iterm tctable env t, iterm tctable env t', reset ())
  (* give x : A = t *)
  | SynTeGive (x, t, t') -> 
      (* Check if identifier free. No shadowing in give *)
      free env x;
      (* Bind, no problem *)
      let x, env' = bind env x in
      TeGive (x, iterm tctable env t, iterm tctable env' t')
  (* 
   *  DESTRUCTORS
   *)
  (* match t return T with (p => M) *)
  | SynTeMatch (t, oty, clauses) ->
      let ioty = match oty with None -> None | Some ty -> Some (itype tctable env ty) in
      TeMatch (iterm tctable env t, ioty, iclauses tctable env clauses, reset ())
  (*
   *  ANNOTATIONS
   *)
  (* x : A *)
  | SynTeTyAnnot (t, ty) -> TeTyAnnot (iterm tctable env t, itype tctable env ty)
  (* t *)
  | SynTeLoc (loc, t) -> TeLoc (loc, iterm tctable env t)
  
and iconstant = function 
  | SynTeOne -> TeOne
  | SynTeTop -> TeTop

and iclauses tctable env clauses =
  List.map (iclause tctable env) clauses

and iclause tctable env = function
  | SynClause (p, t) ->
      let p, env = ipattern dummy tctable env p in
      Clause (p, iterm tctable env t)

and ipattern loc tctable env = function
  (* | SynPatData (loc, d, tyvars, fields) ->
      let env, tyvars = Import.bind_sequentially env tyvars in
      let env, fields = Import.bind_sequentially env fields in
      PatData (loc, Import.resolve env d, tyvars, fields, ref None), env *)
  (* * *)
  | SynPatOne -> PatOne, env
  (* _ *)
  | SynPatWildcard -> PatWildcard, env
  (* x *)
  | SynPatVar x -> 
      (* Check if identifier free. No shadowing in patterns *)
    free env x;
    (* Do our thing *)
    let x, env = bind env x in PatVar x, env
  (* x : A *)
  | SynPatTyAnnot (p, ty) -> 
      let ty = itype tctable env ty in 
      let p, env = ipattern loc tctable env p in
      PatTyAnnot (p, ty), env
  (* p! *)
  | SynPatBang p -> 
      let p, env = ipattern loc tctable env p in
      PatBang p, env
  (* p, p *)
  | SynPatSimPair (p, p') -> 
      (* Ensure linearity *)
      Import.linear (get_id_pattern (SynPatSimPair (p, p')));
      (* Convert *)
      let p, env = ipattern loc tctable env p in
      let p', env = ipattern loc tctable env p' in
      PatSimPair (p, p'), env
  (* <-,p,-> *)
  | SynPatAltPair (bef, p, aft) -> 
      let p, env = ipattern loc tctable env p in
      PatAltPair (bef, p, aft), env
  (* p <: A + _ + A *)
  | SynPatUnion (p, (head_tys, tail_ty)) ->
      let head_tys = itypes tctable env head_tys in
      let tail_ty = itypeo tctable env tail_ty in 
      let p, env = ipattern loc tctable env p in
      PatUnion (p, (head_tys, tail_ty)), env
  (* p | p *)
  | SynPatOr (p, p') -> 
    (* Get identifiers for both sides *)
    let id_left = get_id_pattern p in 
    let id_right = get_id_pattern p' in 
    (* As set *)
    let set_left = IdentifierSet.of_list id_left in
    let set_right = IdentifierSet.of_list id_right in
    (* Compare *)
    if not (IdentifierSet.equal set_left set_right) then 
      error [loc] "Pattern disjunction must have the same variables on both sides";
    (* Ensure linearity *)
    Import.linear id_left;
    (* Bind on any *)
    let p', _ = ipattern loc tctable env p' in 
    let p, env = ipattern loc tctable env p in
    (* Return *)
    PatOr (p, p'), env    
  (* p *)
  | SynPatLoc (loc, p) -> 
      let p, env = ipattern loc tctable env p in 
      PatLoc (loc, p), env


and get_id_pattern = function 
  | SynPatOne | SynPatWildcard -> []
  | SynPatVar x -> [x]
  | SynPatTyAnnot (p, _) | SynPatBang p | SynPatUnion (p, _)
  | SynPatAltPair (_, p, _) | SynPatLoc(_, p) -> get_id_pattern p
  | SynPatSimPair (p, p') | SynPatOr (p, p') -> (get_id_pattern p) @ (get_id_pattern p')

(* ------------------------------------------------------------------------- *)

(* [build] builds the initial toplevel environment, which binds all
   type constructors and all data constructors. It also builds the
   type constructor and data constructor tables. *)

(* let build ds : type_table * datacon_table * env =

  let env = Import.empty in

  (* Gather all type constructors. *)

  let tcs : identifier list =
    List.fold_left (fun tcs -> function SynType (tc, _) -> tc :: tcs | _ -> tcs) [] ds
  in

  (* Build an import environment for type constructors. *)

  let env = Import.bind_simultaneously env tcs in
  Error.signaled();

  (* Build a table that maps a type constructor to its arity. *)

  let tctable : int AtomMap.t =
    List.fold_left (fun tctable -> function
      | SynType (tc, params) -> AtomMap.add (Import.resolve env tc) (List.length params) tctable
      | _ -> tctable
    ) AtomMap.empty ds
  in

  (* Gather all data constructors. *)

  let dcs : identifier list =
    List.fold_left (fun dcs -> function SynDatacon (d, _) -> d :: dcs | _ -> dcs) [] ds
  in

  (* Extend the import environment with the data constructors. *)

  let env = Import.bind_simultaneously env dcs in

  (* Build a table that maps a data constructor to its type scheme. *)

  let dctable : ftype AtomMap.t =
    List.fold_left (fun dctable -> function
      | SynDatacon (d, s) -> AtomMap.add (Import.resolve env d) (ischeme tctable env s) dctable
      | _ -> dctable
    ) AtomMap.empty ds
  in

  tctable, dctable, env *)

(* ------------------------------------------------------------------------- *)

(* [program] converts a complete program. *)

let program = function
  | SynProg t ->
      let t = iterm AtomMap.empty Import.empty t in
      Error.signaled();
      Program (t)

