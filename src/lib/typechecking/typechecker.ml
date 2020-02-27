open Env
open Lang
open Lang.Terms
open Lang.Types
open Utils

(* Infer the type of a program *)
let rec infer 
  (p: pre_program)       (* The program *)
  (xenv: Export.env)     (* The export environment *)
  (env: Env.env)         (* The typing environment *)
  (loc: Error.location)  (* The current location *)
  (term: pre_fterm)      (* The given term *)
    : ftype * env =      (* The term of the type, and 
                            the environment left after inferrence *)
  
  match term with 
    (* Constants *)
    | TeConst TeOne -> TyOne, env
    | TeConst TeTop -> TyTop, env
    (* x *)
    | TeVar (x, metadata) -> 
      (* Try get binding *)
      let env, ty = 
        try Env.retrieve env x
        with 
          | Unbound x -> Typefail.unbound xenv loc x 
          | NoMore x -> Typefail.no_more xenv loc x
      in 
      (* Save metadata *)
      metadata := Some ty;
      (* Return type *)
      ty, env
    
    (* t, t' *)
    | TeSimPair (t1, t2) -> 
      (* Infer type of t1 and t2 in the same environment *)
      let ty1, env1 = infer p xenv env loc t1 in
      let ty2, env2 = infer p xenv env1 loc t2 in
      (* Return t1 & t2 *)
      TyWith (ty1, ty2), env2
      
    (* <t, t'> *)
    | TeAltPair (t1, t2) -> 
      (* Infer type of t1 and t2 in the same environment *)
      let ty1, env1 = infer p xenv env loc t1 in
      let ty2, env2 = infer p xenv env loc t2 in
      (* Check if output environments are the same *)
      if not (Env.equal env1 env2) then 
        Typefail.mismatch_env xenv loc env1 env2;
      (* if not (Env.equal digest1 digest2) then failwith "No equal"; *)
      TyWith (ty1, ty2), env2
      (* v2 *)
      (* Ensure environments have consumed the same variables *)
      (* let digest1 = Env.consumed env env1 in
      let digest2 = Env.consumed env env2 in *)
      (* if not (Env.disjoint digest1 digest2) then failwith "No disjoint";
      TyWith (ty1, ty2), (Env.inter env1 env2) *)

    (* A | t *)
    | TeUnionLeft (t, ty2) ->
      (* Infer type of t *)
      let ty1, env1 = infer p xenv env loc t in
      (* Return ty1 + ty2 *)
      TyPlus (ty1, ty2), env1

    (* t | A *)
    | TeUnionRight (ty1, t) ->
      (* Infer type of t *)
      let ty2, env2 = infer p xenv env loc t in
      (* Return ty1 + ty2 *)
      TyPlus (ty1, ty2), env2

    (* t! *)
    | TeBang t -> 
      (* Infer type of t *)
      let ty, env' = infer p xenv env loc t in
      (* Check if environment is still the same,
         that is, if no linear variable has been consumed *)
      if not (Env.equal env env') then begin 
        (* Pick a linear variable that was consumed *)
        let x, _ = Env.pick (Env.consumed env env') in
        (* Error *)
        Typefail.bang_no_consume xenv loc x
      end;
      (* If exponential, no need to bang. Else, bang *)
      if Types.is_exponential ty 
        then ty, env
        else TyBang ty, env

    (* give x = t1 in t2*)
    | TeGive (x, t1, t2) -> 
      (* Extend xenv *)
      let xenv = Export.bind xenv x in
      (* Infer type of t1 *)
      let ty1, env1 = infer p xenv env loc t1 in
      (* Try bind binding *)
      let env2 = 
        try Env.bind env1 x ty1
        with 
          | Bound (x, _) -> Typefail.bound xenv loc x
      in 
      (* Bind x:t1 in t2 and infer type of t2 *)
      infer p xenv env2 loc t2

    (* x : A *)
    | TeTyAnnot (t, ty) -> ty, check p xenv env loc t ty
    (* t *)
    | TeLoc (loc, t) -> infer p xenv env loc t

    (* Default *)
    | _ -> failwith "TODO"

(* Check the type of a prograù *)
and check
  (p: pre_program)       (* The program *)
  (xenv: Export.env)     (* The export environment *)
  (env: Env.env)         (* The typing environment *)
  (loc: Error.location)  (* The current location *)
  (term: pre_fterm)      (* The given term *)
  (expected: ftype)      (* The expected type *)
    : env =             
  
  match term with
    | TeLoc (loc, term) ->
        (* Infer type from expression *)
        let inferred, env = infer p xenv env loc term in
        (* Verify type equals inferred type *)
        let types_equal = Types.equal inferred expected in
        (* If not equal types *)
        if not types_equal then
          (* Throw error *)
          Typefail.mismatch xenv loc expected inferred;
          (* Return modified env *)
          env
    | _ ->
      (* out of luck! We run in degraded mode, location will be wrong! *)
        let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos) in
        check p xenv env loc (TeLoc (dummy_loc, term)) expected

(* A complete program is typechecked within empty environments. *)
let run (Program (term) as p : pre_program) =
  (* Make empty environments & location *)
  let xenv = Export.empty in 
  let env = Env.empty in
  let loc = Error.dummy in 
  (* Add basic type ctors *)
  (* let xenv = AtomMap.fold (fun tc _ xenv -> Export.bind xenv tc) tctable xenv in
  let xenv = AtomMap.fold (fun dc _ xenv -> Export.bind xenv dc) dctable xenv in *)
  (* Infer type *)
  let ty, env = infer p xenv env loc term in
  (* Ensure new context is empty *)
  if not (Env.is_empty env) then 
    Typefail.unused xenv loc (Env.pick env);
  (* Return env *)
  xenv, ty

(* let type_of (term: fterm) = match term with 
  (* Constants *)
  | TeConst (TeOne) -> TyOne
  | TeConst (TeTop) -> TyTop 
  (* Variable *)
  | TeVar (_, ty) -> ty

  | _ -> failwith "" *)
