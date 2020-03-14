open Lang
open Lang.Sort
open Lang.Types
open Lang.Terms
open Printf
open Syntax
open Utils
open Utils.Atom
open Utils.Error
open Utils.Identifier

type atom =
    Atom.atom

type env =
    Import.env


(* ------------------------------------------------------------------------- *)
(* Locate a syntaxic term *)
let locate ?(default=Error.dummy) = function 
  | SynTeLoc (loc, _) -> loc 
  | _ -> default

(* ------------------------------------------------------------------------- *)

(* Term variables, type variables, type constructors, and data constructors are
   explicitly bound. *)
let bind env id : atom * env =
  let env = Import.bind env id in
  Import.resolve env id, env

let free = Import.free

let bind_no_dupl env id : atom * env =
  free env id;
  bind env id

(* ------------------------------------------------------------------------- *)

let itycon tctable env id tys = 
  let tc = Import.resolve env id in
  let arity_got = List.length tys in
  let arity_expected = 
    (* If found, check arity. Else, fail silently *)
    Option.value (AtomMap.find_opt tc tctable) ~default:arity_got 
  in
  (* Compare arities *)
  if arity_got <> arity_expected then 
    Error.signal [ Identifier.location id ]
      (Printf.sprintf "The type constructor %s expects %d arguments, but is here applied to %d arguments.\n"
        (Identifier.name id) arity_expected arity_got);
  (* Return the tycon *)
  TyCon (tc, tys)

(* [itype] converts a type from external syntax into internal syntax. *)
let ityvar env a =
  TyFreeVar (Import.resolve env a)

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
  | SynTyForall (a, body) -> 
      let a, env = bind env a in 
      TyForall (Types.abstract a (itype tctable env body))
      
  (* Type free variables / type constructors *)
  | SynTyVarOrTyCon (id, []) when Import.resolves env type_sort id ->
      (* if there are no type arguments and if [id] resolves as a type variable [a],
         then consider it is a type variable *)
      let a = Identifier.mak type_sort id in ityvar env a
      
  | SynTyVarOrTyCon (id, tys) -> 
      (* This is a type constructor *)
      let tc = Identifier.mak typecon_sort id in
      itycon tctable env tc (itypes tctable env tys)

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
  (* Dtycon *)
  | SynTeData id -> TeData (Import.resolve env id, reset ())
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
  (* forall [A] -> t *)
  | SynTeTyAbs (a, t) -> 
    (* Shadowing allowed in type abstractions *)
    let a, env = bind env a in
    TeTyAbs (a, iterm tctable env t)
  (* t [A] *)
  | SynTeTyApp (t, ty) -> TeTyApp (iterm tctable env t, itype tctable env ty, ref None)
  (* give x : A = t *)
  | SynTeGive (x, t, t') -> 
      (* Check if identifier free. No shadowing in give *)
      (* free env x; *)
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
let itydecl tytable ienv = function 
  (* GADT decl *)
  | SynDtyGADT (tyname, tyvars, ctors) -> 
      (* Check if free & bind type ctors name *)
      let tyname, ienv = bind_no_dupl ienv tyname in
      (* Bind all tyvars *)
      let tyvars, tyvars_env =
        let tyvars_env = Import.bind_simultaneously ienv tyvars in
        let tyvars = List.map (Import.resolve tyvars_env) tyvars in
        tyvars, tyvars_env
      in       
      (* Check if free & bind data ctors names *)
      let ctors, ienv = 
        (* Split ctor names and teir types *)
        let ctors, tys = List.split ctors in
        (* Check all ids are free *)
        List.iter (free ienv) ctors;
        (* Bind all identifiers *)
        let ienv = Import.bind_simultaneously ienv ctors in
        (* Resolve them to get matching atoms *)
        let ctors = List.map (Import.resolve ienv) ctors in
        (* Resolve all types, but use the environment where
           we internalized the type vars *)
        let tys = List.map (itype tytable tyvars_env) tys in
        (* Recombine, and return env w/ all the ctors *)
        List.combine ctors tys, ienv
      in
      (* Map the tycon to its arity *)
      let arity = List.length tyvars in
      let tytable = AtomMap.add tyname arity tytable in
      (* Return *)
      DtyGADT (tyname, tyvars, ctors), (ienv, tytable)

(* ------------------------------------------------------------------------- *)
let itedecl tytable ienv = function 
  (* Linear decl *)
  | SynDteLinear (p, t) ->
      (* Internalize term & pattern*)
      let t = iterm tytable ienv t in
      let p', ienv = ipattern (Error.dummy) tytable ienv p in
      (* Return *)
      DteLinear (p', t), ienv
      

(* ------------------------------------------------------------------------- *)

let idecl toplevel tytable ienv = function 
    (* Top level declaration with disabled toplevel mode*)
    | SynDeclTop t when not toplevel -> 
        Error.error [locate t] 
          (sprintf "Terms cannot be evaluated without being bound when toplevel mode is disabled.")
    (* Top level declaration with disabled toplevel mode*)
    | SynDeclTop t -> DeclTop (iterm tytable ienv t, reset ()), (tytable, ienv)
    (* Type decl *)
    | SynDeclType tydecl -> 
        let tydecl, (ienv, tytable) = itydecl tytable ienv tydecl in 
        DeclType (tydecl, reset (), reset ()), (tytable, ienv)
    (* Term decl *)
    | SynDeclTerm tedecl -> 
        let tedecl, ienv = itedecl tytable ienv tedecl in
        DeclTerm tedecl, (tytable, ienv)

let idecls toplevel ienv decls = 
  (* Internalize declarations *)
  let decls, (_, ienv) = 
    let idecl_folder (decls, (tytable, ienv)) decl = 
      (* Internalize this declaration *)
      let decl, (tytable, ienv) = idecl toplevel tytable ienv decl in
      (* Signal error *)
      Error.signaled ();
      (* Return *)
      (decl::decls, (tytable, ienv))
    in
    List.fold_left idecl_folder ([], (AtomMap.empty, ienv)) decls 
  in 
  (* Return converted program *)
  ienv, decls

(* ------------------------------------------------------------------------- *)

(* [declaration] converts a single declaration *)

let declaration ienv decl = 
  let decl, (_, ienv) = idecl true AtomMap.empty ienv decl in
  (* Signal error *)
  Error.signaled ();
  (* Return *) 
  ienv, decl

let declarations ienv decls = 
  (* Internalize declarations *)
  let decls, (_, ienv) = 
    let idecl_folder (decls, (tytable, ienv)) decl = 
      (* Internalize this declaration *)
      let decl, (tytable, ienv) = idecl true tytable ienv decl in
      (* Signal error *)
      Error.signaled ();
      (* Return *)
      (decl::decls, (tytable, ienv))
    in
    List.fold_left idecl_folder ([], (AtomMap.empty, ienv)) decls 
  in 
  (* Return converted program *)
  ienv, decls

(* [program] converts a complete program. *)

let program = function
  | SynProg decls -> 
    let _, decls = idecls false Import.empty decls in
    Program (decls, reset ())
