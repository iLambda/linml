open Env
open Lang
open Lang.Terms
open Lang.Types
open Lang.Kinds
open Printf
open Utils

(* ------------------------------------------------------------------------------- *)
(* Exceptions *)
exception NoInfer of (unit -> unit)

(* ------------------------------------------------------------------------------- *)
(* Helpers *)

(* Locate the term *)
let locate default = 
  function 
    | TeLoc (loc, _) -> loc
    | _ -> default

(* Locate the term *)
let locate_atom x = 
  Identifier.location (Atom.identifier x)
  
(* Destruct a function type *)
let destruct_lollipop xenv loc = function 
| TyLollipop (domain, codomain) -> domain, codomain 
| t -> Typefail.expected_form xenv loc "a linear application" t

(* Make a fresh identifier *)
let fresh () = Atom.fresh (Identifier.mk "x" Sort.term_sort)

(* Deconstruct a quantified type *)
let deconstruct_univ xenv loc =
  function 
    | TyForall ctx -> ctx
    | ty -> Typefail.expected_form xenv loc "a universal" ty

(* Check type is well formed, but catch exception *)
let well_formed xenv ktbl ty = 
  try 
    Kinds.well_formed ktbl ty
  with 
  | Tyvar_unbound x -> 
      Typefail.unbound_id_kind x "type variable"
  | Tycon_unbound x -> 
      Typefail.unbound_id_kind x "type constructor"
  | Tycon_arity { tycon; expected; got } ->
      Typefail.tycon_arity_mismatch xenv tycon expected got

(* ------------------------------------------------------------------------------- *)
(* Strictness of judgements *)

(* Type of strictness *)
type strictness = 
  | Strict
  | Slack

(* b \/ b' *)
let slackest x y = 
  match x, y with  
    | Strict, Strict -> Strict
    | Strict, Slack -> Slack
    | Slack, Strict -> Slack
    | Slack, Slack -> Slack

(* b /\ b *)
let strictest x y = 
  match x, y with  
    | Strict, Strict -> Strict
    | Strict, Slack -> Strict
    | Slack, Strict -> Strict
    | Slack, Slack -> Slack

(* ------------------------------------------------------------------------------- *)
(* Typechecking a term *)

(* Infer the type of a program *)
let rec infer 
  (ktbl: Kinds.env)       (* The table of types&kinds *)
  (xenv: Export.env)     (* The export environment *)
  (env: Env.env)         (* The typing environment *)
  (loc: Error.location)  (* The current location *)
  (term: pre_fterm)      (* The given term *)
    : ftype * (linenv * strictness) =      (* The term of the type,  
                                            the output environment,
                                            the modality of the output *)
  
  match term with 
    (* Constants *)
    | TeConst TeOne -> TyOne, (Env.linearize env, Strict)
    | TeConst TeTop -> TyTop, (Env.linearize env, Slack)
    (* x *)
    | TeVar (x, metadata) -> 
      (* Try get binding *)
      let env, ty = 
        try Env.consume env x
        with 
          | Unbound x -> Typefail.unbound xenv loc x 
          | NoMore x -> Typefail.no_more xenv loc x
      in 
      (* Save metadata *)
      metadata := Some ty;
      (* Return type *)
      ty, (Env.linearize env, Strict)
    (* Dtycon *)
    | TeData (x, metadata) -> 
        (* Try lookup the datatype constructor *)
        let dty = 
          try Kinds.lookup_dtycon ktbl x 
          with | Dtycon_unbound x -> 
            Typefail.unbound_id_kind x "data constructor"  
        in 
        (* Save metadata *)
        metadata := Some dty;
        (* Return type *)
        dty, (Env.linearize env, Strict)
    
    (* t, t' *)
    | TeSimPair (t1, t2) -> 
      (* Infer type of t1 and t2 in the same environment *)
      let ty1, (lenv1, mod1) = infer ktbl xenv env loc t1 in
      let ty2, (lenv2, mod2) = infer ktbl xenv (Env.compose lenv1 env) loc t2 in
      (* Return t1 & t2 *)
      TyTensor (ty1, ty2), (lenv2, slackest mod1 mod2)
      
    (* <t, t'> *)
    | TeAltPair (t1, t2) -> 
      (* Infer type of t1 and t2 in the same environment *)
      let ty1, (lenv1, mod1) = infer ktbl xenv env loc t1 in
      let ty2, (lenv2, mod2) = infer ktbl xenv env loc t2 in
      (* Check if output environments are the same *)
      if not (Env.equal lenv1 lenv2) then begin 
        (* Separate the environments *)
        let x, reason = Env.separate lenv1 lenv2 in
        (* Get location for the branches *)
        let loc1 = locate loc t1 in 
        let loc2 = locate loc t2 in
        (* Error *)
        Typefail.mismatch_additive xenv loc1 loc2 x reason
      end;
      (* Return type *)
      TyWith (ty1, ty2), (lenv2, strictest mod1 mod2)

    (* A | t *)
    | TeUnionLeft (t, ty2) ->
      (* Infer type of t *)
      let ty1, (lenv1, mod1) = infer ktbl xenv env loc t in
      (* Ensure type is well formed *)
      well_formed xenv ktbl ty2;
      (* Return ty1 + ty2 *)
      TyPlus (ty1, ty2), (lenv1, mod1)

    (* t | A *)
    | TeUnionRight (ty1, t) ->
      (* Infer type of t *)
      let ty2, (lenv2, mod2) = infer ktbl xenv env loc t in
      (* Ensure type is well formed *)
      well_formed xenv ktbl ty1;
      (* Return ty1 + ty2 *)
      TyPlus (ty1, ty2), (lenv2, mod2)

    (* t! *)
    | TeBang (t, metadata) -> 
      (* Linearize environment *)
      let lenv = Env.linearize env in
      (* Infer type of t with empty linear bindings *)
      let ty, (lenv', _) = infer ktbl xenv env loc t in
      (* Check output type *)
      let output_ty = 
        match ty with 
          (* Banged. Just return *)
          | TyBang ty -> ty
          (* Not banged. Type *)
          | _ -> 
            (* Check if linear environment has been untouched *)
            if not (Env.equal lenv lenv') then begin 
              (* Pick a linear variable that was manipulated and throw an error *)
              let x, _ = Env.pick (Env.difference lenv lenv') in
              Typefail.no_linear xenv loc "banged term" x
            end;
            (* Return types *)
            TyBang ty
        in
      (* Save metadata *)
      metadata := Some output_ty;
      (* Return type and environmnet *)
      output_ty, (lenv, Strict)

    (* refute with t *)
    | TeZero _ -> 
        (* Cannot infer type of a non annotated zero-elim. *)       
        (* This kind of value can only be checked, hence surrounded 
           by a type annotation *)
        raise (NoInfer (fun () -> Typefail.cant_infer_refute xenv loc))
        
    (* (x : A) -o t *)
    | TeLinAbs (x, dom, t) -> 
      (* Introduct type argument into export env *)
      let xenv = Export.bind xenv x in 
      (* Split the environment over x *)
      let clear_lenv, leftover_lenv = Env.split (Env.linearize env) x in
      (* Ensure type is well formed *)
      well_formed xenv ktbl dom;
      (* Bind x to evaluate body *)
      let env1 = 
        (* Try to bind *)
        try Env.bind (Env.compose clear_lenv env) x dom 
        with
          (* Can't happen, since the env is split *)
          | Bound _ -> assert false
          (* We bound a top. *)
          | NoBindTop _ -> Typefail.no_bind_top xenv (locate_atom x) "appear as a function parameter"
      in
      (* Typecheck the function body *)
      let codom, (lenv2, mod2) = infer ktbl xenv env1 loc t in
      (* If modality is strict and variable has been unused, error *)
      if mod2 = Strict && Env.has lenv2 x then
        Typefail.unused xenv loc (x, Env.multiplicity lenv2 x);
      (* Remove the argument binding from the context and merge with leftover binding *)
      let final_lenv = Env.merge 
                        (Env.purge lenv2 x)
                        leftover_lenv in     
      (* Type is function.*)
      TyLollipop(dom, codom), (final_lenv, mod2)

    (* t t' *)
    | TeApp (t, t', metadata) ->
      (* Evaluate the application *)
      let fty, (lenv1, mod1) = infer ktbl xenv env loc t in
      (* If application is banged, unbang *)
      let fty, bang = match fty with 
        | TyBang fty -> fty, true  
        | fty        -> fty, false in
      (* Deconstruct arrow & save metadata *)
      let dom, codom = destruct_lollipop xenv loc fty in
      metadata := Some { domain = dom; codomain = codom; bang = bang };
      (* Check that argument is of domain type *)
      let lenv2, mod2 = check ktbl xenv (Env.compose lenv1 env) loc t' dom in
      (* Return codomain *)
      codom, (lenv2, slackest mod1 mod2)

    (* forall [A] -> t *)
    | TeTyAbs (x, t) -> 
      (* Introduct type argument into export env *)
      let xenv = Export.bind xenv x in  
      (* Introduce type variable *)
      let ktbl = Kinds.fv_add ktbl x in
      (* Infer type of body *)
      let body_ty, (body_lenv, body_mod) = infer ktbl xenv env loc t in
      (* Make type context and return *)
      TyForall (Types.abstract x body_ty), (body_lenv, body_mod)

    (* t [A] *)
    | TeTyApp (t, ty, metadata) -> 
      (* Infer type of body *)
      let body_ty, (body_lenv, body_mod) = infer ktbl xenv env loc t in
      (* Get type context *)
      let ty_ctx = deconstruct_univ xenv loc body_ty in
      (* Save type context info *)
      metadata := Some { context = ty_ctx };
      (* Ensure type is well formed *)
      well_formed xenv ktbl ty;
      (* Fill the type substitution and return the produced type *)
      Types.fill ty_ctx ty, (body_lenv, body_mod)

    (* give x = t1 in t2*)
    | TeGive (x, t1, t2) -> 
      (* Infer type of t1 *)
      let ty_arg, (lenv_arg, mod_arg) = infer ktbl xenv env loc t1 in
      
      (* Split the environment over x *)
      let lenv_clear, lenv_leftover = Env.split lenv_arg x in
      (* Bind x to evaluate body *)
      let env1 = 
        (* Try to bind *)
        try Env.bind (Env.compose lenv_clear env) x ty_arg 
        with
          (* Can't happen, since the env is split *)
          | Bound _ -> assert false
          (* We bound a top. *)
          | NoBindTop _ -> Typefail.no_bind_top xenv (locate_atom x) "be bound"
      in
      (* Extend export env *)
      let xenv = Export.bind xenv x in
      (* Infer the give's body type *)
      let ty_body, (lenv_body, mod_body) = infer ktbl xenv env1 loc t2 in
      (* If modality is strict and variable has been unused, error *)
      if mod_body = Strict && Env.has lenv_body x then
        Typefail.unused xenv loc (x, Env.multiplicity lenv_body x);
      (* Remove the argument binding from the context and merge with leftover binding *)
      let final_lenv = Env.merge 
                        (Env.purge lenv_body x)
                        lenv_leftover in     
      (* Type is body *)
      ty_body, (final_lenv, slackest mod_arg mod_body)
      
    (* match t return T with | *)
    | TeMatch (t, oty, clauses, metadata) -> 
      (* Infer type of scrutinee *)
      let scrutinee_ty, (scrutinee_lenv, scrutinee_mod) = infer ktbl xenv env loc t in
      let scrutinee_env = Env.compose scrutinee_lenv env in
      (* Check if top *)
      if Types.equal scrutinee_ty TyTop then 
        Typefail.no_bind_top xenv (locate loc t) "be the scrutinee of a match";

      (* Evaluate patterns *)
      let pattern_binder = (Fun.flip (pattern ktbl xenv scrutinee_lenv loc)) scrutinee_ty in
      let patterns = List.map (fun (Clause (pat, _)) -> pat) clauses in
      let patterns_bindings = List.map pattern_binder patterns in
      let patterns_envs = List.map (Env.binds scrutinee_env) patterns_bindings in

      (* Check that patterns are exhaustive *)
      exhaustive patterns;

      (* Obtain return type *)
      let body_ty = match oty with 
        (* Match has return statement *)
        | Some ty -> 
          (* Ensure type is well formed *)
          well_formed xenv ktbl ty;
          (* Return it *)
          ty
        (* Infer from first available clause *)
        | None -> 
          (* Helper to infer type from body *)
          let infer_from_branch oty env (Clause (_, body)) = 
            (* If type already found, just pass *)
            if Option.is_some oty then oty
            (* Not found ; try to infer *)
            else 
              (* Try to infer *)
              begin 
                try 
                  (* Infer *)
                  let ty, _ = infer ktbl xenv env loc body in 
                  (* Return inferred type *)
                  Some ty
                (* Couldn't infer ; try next one *)
                with NoInfer _ -> None 
              end 
          in             
          (* Try get the first one can be inferred *)
          let inferred_oty = List.fold_left2 infer_from_branch None patterns_envs clauses in
          (* Check if inferrence worked *)
          match inferred_oty with 
            (* Couldn't infer. Error *)
            | None -> raise (NoInfer (fun () -> Typefail.cant_infer_match xenv loc))
            (* Inferrence was successful. Return *)
            | Some ty -> ty
      in
      (* Evaluate all bodies *)
      let body_checker env body = check ktbl xenv env loc body body_ty in
      let bodies = List.map (fun (Clause (_, b)) -> b) clauses in 
      let bodies_inferred = List.map2 body_checker patterns_envs bodies in
      let bodies_envs, bodies_mods = List.split bodies_inferred in

      (* Check if all bindings have been consumed, if modality is strict *)
      let consumption_check (env, modality) bindings = 
        (* If modality is strict and bindings are left *)
        if modality = Strict then begin
          (* Compute intersection of bindings and linear environment *)
          let bindings_left = Env.inter (Env.of_bindings bindings) env in
          (* Check if it is empty *)
          if not (Env.is_empty bindings_left) then 
            (* Pick one variable and error *)
            Typefail.unused xenv loc (Env.pick bindings_left);
        end
      in
      List.iter2 consumption_check bodies_inferred patterns_bindings;
      
      (* Compute the return environment *)
      let return_env = 
        (* Compute the end environent of each body without the bindings *)
        let return_body_env = List.map2 Env.purges bodies_envs patterns_bindings in
        (* Intersect all of them *)
        List.fold_left Env.inter (List.hd return_body_env) (List.tl return_body_env)          
      in
      (* Compute return modality ; it is i \/ j_0 \/ j_1 \/ ... *)
      let return_mod = List.fold_left slackest scrutinee_mod bodies_mods in
        
      (* Save return_type metadata *)
      metadata := Some body_ty;
      (* Return type of return *)
      body_ty, (return_env, return_mod)

    (* x : A *)
    | TeTyAnnot (t, ty) -> 
      (* Ensure type is well formed *)
      well_formed xenv ktbl ty;
      (* Return it *)
      ty, check ktbl xenv env loc t ty
    (* t *)
    | TeLoc (loc, t) -> infer ktbl xenv env loc t

(* Check the type of a prograù *)
and check
  (ktbl: Kinds.env)   (* The table of types&kinds *)
  (xenv: Export.env)     (* The export environment *)
  (env: Env.env)         (* The typing environment *)
  (loc: Error.location)  (* The current location *)
  (term: pre_fterm)      (* The given term *)
  (expected: ftype)      (* The expected type *)
    : linenv * strictness =        (* The term of the type,  
                                    the output environment,
                                    the modality of the output *)
  
  match term with
    (* refute with t *)
    | TeLoc (loc, TeZero (t, info)) -> 
      (* Evaluate t, and check it is zero *)
      let zero_lenv, _ = check ktbl xenv env loc t TyZero in
      (* Set metadata *)
      info := Some expected;
      (* Check is always successfull, since zero is polymorphic *)
      zero_lenv, Slack

    (* Usual term *)
    | TeLoc (loc, term) ->
        (* Infer type from expression *)
        let inferred, (env, m) = infer ktbl xenv env loc term in
        (* Verify type equals inferred type *)
        let types_equal = Types.equal inferred expected in
        (* If not equal types *)
        if not types_equal then
          (* Throw error *)
          Typefail.mismatch xenv loc expected inferred;
          (* Return modified env *)
          env, m

    | _ ->
      (* out of luck! We run in degraded mode, location will be wrong! *)
        let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos) in
        check ktbl xenv env loc (TeLoc (dummy_loc, term)) expected

(* ------------------------------------------------------------------------------- *)
(* Typechecking a pattern *)

(* Check the type of a pattern *)
and pattern
  (ktbl: Kinds.env)       (* The table of types&kinds *)
  (xenv: Export.env)     (* The export environment *)
  (lenv: Env.linenv)     (* The typing environment *)
  (loc: Error.location)  (* The current location *)
  (pat: pattern)         (* The given pattern *)
  (ty: ftype)            (* The expected type *)
    : Bindings.t =     (* Returns bindings to add *)
  
  match pat with 
    (* * *)
    | PatOne -> 
      (* Check if type is one *)
      if not (Types.equal ty TyOne) then 
        Typefail.mismatch xenv loc TyOne ty;
      (* Type is one. Return no binding *)
      Bindings.empty
      
    (* _ *)
    | PatWildcard -> 
      (* Check if type is exponential *)
      if not (Types.is_exponential ty) then 
        (* Not exponential. Discard *)
        Typefail.no_discard_linear xenv loc ty;
      (* Discard, return no binding *)
      Bindings.empty

    (* x *)
    | PatVar x ->
      (* If type is linear and variable already bound, error *)
      if not (Types.is_exponential ty) && Env.has lenv x then 
        Typefail.bound xenv loc x;
      (* Check if type is top *)
      if Types.equal ty TyTop 
      then Typefail.no_bind_top xenv (locate_atom x) "appear in a pattern"
      else Bindings.singleton (x, ty)   (* Bind variable, strict *)

    (* Data *)
    | PatData (*(x, tyvars, ps)*) _ ->
      failwith "TODO"
      (* Ensures dtycon exists *)
      (* if not (Kinds.has_dtycon ktbl x) then 
        Typefail.unbound_id_kind x "data constructor";
      (* Find its tycon (can't fail, since it exists) *)
      let tycon_name, _ = Kinds.find_tycon ktbl x in

      (* Check type is the right tycon *)
      begin match ty with 
        (* OK *)
        | TyCon (expected_tycon, _) when Atom.equal expected_tycon tycon_name -> ()
        (* Error *)
        | TyCon (expected_tycon, _) -> Typefail.tycon_mismatch xenv loc expected_tycon tycon_name
        | _ -> Typefail.expected_form xenv loc "type constructor" ty
      end;
      
      (* Check tycon arity is ok *)
      let arity_tycon = Kinds.arity_tycon ktbl tycon_name in
      if List.compare_length_with tyvars arity_tycon <> 0 then 
        Typefail.tycon_arity_mismatch xenv 
          tycon_name arity_tycon (List.length tyvars);  
      (* Add tyvars to kinds table *)
      let ktbl' = Kinds.fv_adds ktbl tyvars in

      (* Check dtycon arity is ok *)
      let arity_dtycon = Kinds.arity_dtycon ktbl x in
      if List.compare_length_with ps arity_dtycon <> 0 then 
        Typefail.dtycon_arity_mismatch xenv 
          x arity_dtycon (List.length ps);        
      (* Get binding of all subpatterns *)
      let bindings = List.map2 (pattern ktbl' xenv lenv loc) ps tys in
      (* Fuse them together, and return *)
      List.fold_left Bindings.union Bindings.empty bindings *)

    (* !x *)
    | PatBang pat ->
      (* Check type is a banged one *)
      begin match ty with 
        (* OK *)
        | TyBang ty -> 
            (* Analyse subpattern *)
            pattern ktbl xenv lenv loc pat ty
        (* Error *)
        | _ -> Typefail.expected_form xenv loc "exponential" ty
      end

    (* p, p *)
    | PatSimPair (pat, pat') ->
      (* Check type is a pair *)
      begin match ty with 
        (* OK *)
        | TyTensor (ty, ty') ->  
          (* Check both subpatterns *)
          let b1 = pattern ktbl xenv lenv loc pat ty in
          let b2 = pattern ktbl xenv lenv loc pat' ty' in
          (* Verify they are disjoint  *)
          let b12 = Bindings.inter b1 b2 in
          if not (Bindings.is_empty b12) then begin
            (* Pick one variable from intersection *)
            let x, _ = Bindings.choose b12 in
            (* Error *)
            Typefail.nonlinear_pattern xenv loc x
          end;
          (* Return union *)
          Bindings.union b1 b2
        (* Error *)
        | _ -> Typefail.expected_form xenv loc "tensor" ty
      end
      
    (* <-,p,-> *)
    | PatAltPair (left, pat, right) ->
      (* Check this is indeed a with *)
      begin match ty with 
        | TyWith _ -> ()
        | _ -> Typefail.expected_form xenv loc "with" ty
      end;
      (* Helper *)
      let sty, num_fields =
        let rec aux pat_ty n = function 
          (* If this is a with, iterate *)
          | TyWith (pat_ty, tail) when n = left -> aux (Some pat_ty) (n+1) tail 
          | TyWith (_, tail) -> aux pat_ty (n+1) tail
          (* Else, stop *)
          | pat_ty when n = left -> (Some pat_ty), n
          | _                    -> pat_ty, n
        in 
        (* Call *)
        aux None 0 ty
      in 
      (* Check number of fields *)
      if num_fields <> left + right then 
        Typefail.form_mismatch loc 
          (sprintf "with, with %d fields" (left + right + 1))
          (sprintf "%d" (num_fields + 1));
      (* Force get subpattern type *)
      let sty = Option.get sty in  
      (* Check subpattern *)
      pattern ktbl xenv lenv loc pat sty

    (* p <: A + _ + A *)
    | PatUnion (pat, (left, oright)) -> 
      (* Ensure type is well formed *)
      List.iter (well_formed xenv ktbl) left;
      Option.iter (well_formed xenv ktbl) oright;
      (* Count types *)
      let rec count_oty_plus = function None -> 0 | Some t -> count_ty_plus t
      and count_ty_plus ty = 
        let rec count_ty_plus_aux n = function 
          | TyPlus (_, tail) -> count_ty_plus_aux (n+1) tail
          | _ -> n+1
        in count_ty_plus_aux 0 ty
      in
      (* Check number : need to have the exact same arity *)
      let expected_num = count_ty_plus ty in 
      let got_num = (List.length left) + (count_oty_plus oright) + 1 in
      if expected_num <> got_num then 
        Typefail.form_mismatch loc 
          (sprintf "plus with %d fields in pattern" expected_num)
          (sprintf "%d" got_num);
      (* Get subpattern type *)
      let subty =
        (* Try get the subpattern type *)
        let osubty = 
          let rec get_osubty n = function 
            | TyPlus (_, tail) when n > 0 -> get_osubty (n-1) tail
            | TyPlus (ty, _)   when n = 0 -> Some ty
            | ty when n = 0 -> Some (ty) 
            | _ -> None
          in get_osubty (List.length left) ty
        in 
        (* Check if there is one. *)
        Option.get osubty
        (* match osubty with
          (* Arity mismatch *) 
          | None -> assert false
              (* Check number : need just to have < arity *)
              (* let expected_num = (List.length left) + (count_oty_plus oright) + 1 in 
              let got_num = count_ty_plus ty in
              Typefail.form_mismatch loc 
                (sprintf "plus, with %d fields" got_num)
                (sprintf "%d" expected_num) *)
          (* Get type *)
          | Some subty -> subty *)
      in
      (* Recompose expected type of pattern *)
      let expected_ty = 
        (* Compute type of tail *)
        let tail_ty = match oright with 
          | None -> subty
          | Some tail_ty -> TyPlus (subty, tail_ty)
        in
        (* Rebuild whole type *)
        List.fold_left 
          (fun tail ty -> TyPlus (ty, tail)) 
          tail_ty
          (List.rev left) 
      in
      (* Check type *)
      if (not (Types.equal expected_ty ty)) then 
        Typefail.mismatch xenv loc expected_ty ty;
      (* Type is ok. Now, check ktblattern in question matches subpattern ty type *)
      pattern ktbl xenv lenv loc pat subty
      
    (* p | p *)
    | PatOr (pat, pat') ->
      (* Check both subpatterns *)
      let b1 = pattern ktbl xenv lenv loc pat ty in
      let b2 = pattern ktbl xenv lenv loc pat' ty in
      (* Verify they are equal  *)
      if not (Bindings.equal b1 b2) then begin
        (* Pick one variable from disjunction *)
        let x, _ = 
          Bindings.choose 
            (Bindings.union
              (Bindings.diff b1 b2)
              (Bindings.diff b2 b1))
           in
        (* Error *)
        Typefail.no_uniform_branch_unbound ~linear:false xenv loc x
      end;
      (* Return union *)
      Bindings.union b1 b2

    (* x : t *)
    | PatTyAnnot (pat, ty) -> 
      (* Ensure type is well formed *)
      well_formed xenv ktbl ty;
      (* Return *)
      pattern ktbl xenv lenv loc pat ty
      
    (* p *)
    | PatLoc (loc, pat) -> pattern ktbl xenv lenv loc pat ty

(* Returns true iff the pattern [q] covers patterns [ps] *)
(* and covering q ps =
  match q, ps with 
    (* Not exhaustive *)
    | _, [] -> false

    (* Base cases *)
    | PatVar _, [PatOne]            (*  x covering { * } *)
    | PatVar _, [PatVar _]          (*  x covering { y } *)
    | PatVar _, [PatWildcard]       (*  x covering { _ } *)
        -> true

        

    (* Obvious case *)
    | PatBang q', PatBang p'::ps -> covering q' [p'] && covering q ps
    | PatAltPair (_, q', _), PatAltPair(_,p',_)::ps -> covering q' [p'] && covering q ps

    (* q covering { p | p', ... } := q covering { p, p', ... } *)
    | q, PatOr(p, p')::ps -> covering q (p::p'::ps)
    (* q covering { loc(p), ... } := q covering { p, ... } *)
    | q, PatLoc(_, p)::ps -> covering q (p::ps)
    (* q covering { p : A, ... } := q covering { p, ... } *)
    | q, PatTyAnnot(p, _)::ps -> covering q (p::ps)

    | _ -> failwith "" *)
  
and exhaustive (_patterns: pattern list) = 
  (* Check if covering *)
  (* covering (PatVar (fresh ())) patterns *)
  let _x = fresh () in ()

(* ------------------------------------------------------------------------------- *)
(* Typechecking a declaration *)

(* Check the validity of a type declaration *)
and type_declaration
  (ktbl: Kinds.env)  (* The table of types&kinds *)
  (xenv: Export.env)        (* The export environment *)
  (decl: type_decl)            (* The given type declaration *)
    : Kinds.type_ctor * Kinds.env * Export.env =     (* Returns table of kinds and export env *)

  match decl with 
    (* A GADT declaration *)
    | DtyGADT (tyname, tyvars, ctors) ->
      (* Make dtycons & tycon *)
      let dtycons = List.map (fun (x, ty) -> Constructor (x, ty)) ctors in
      let tycon = TyconGADT (tyvars, dtycons) in 
      (* Extend env *)
      let xenv = Export.bind xenv tyname in
      let xenv = List.fold_left Export.bind xenv (List.map fst ctors) in 
      let xenv = List.fold_left Export.bind xenv tyvars in
      (* Register tycon *)
      let ktbl = 
        try Kinds.register_tycon ktbl tyname tycon 
        with 
          (* Dtycon was already defined *)
          | Dtycon_bound x -> 
              Typefail.dtycon_bound xenv ktbl x
          (* Tycon was already defined *)
          | Tycon_bound x ->
              Typefail.tycon_bound xenv x
          (* Somewhere, a tyvar wasn't bound respected *)
          | Tyvar_unbound x -> 
              Typefail.unbound_id_kind x "type variable"
          (* Somewhere, a typecon wasn't bound *)
          | Tycon_unbound x -> 
              Typefail.unbound_id_kind x "type constructor"
          (* Somewhere, a typecon arity wasn't respected *)
          | Tycon_arity { tycon; expected; got } ->
              Typefail.tycon_arity_mismatch xenv tycon expected got
          (* Type scheme is invalid *)
          | Invalid_type_scheme (dtycon, ty) -> 
              Typefail.invalid_type_scheme xenv dtycon ty 
          (* Type scheme last ctor is invalid *)
          | Invalid_type_scheme_con { dtycon; expected; got } -> 
              Typefail.invalid_type_scheme_tycon xenv dtycon expected got
      in
      (* Return *)
      (Kinds.lookup_tycon ktbl tyname), ktbl, xenv

(* Check the validity of a type declaration *)
and term_declaration
  (ktbl: Kinds.env)  (* The table of types&kinds *)
  (xenv: Export.env)        (* The export environment *)
  (env: Env.env)        (* The export environment *)
  (decl: pre_term_decl)            (* The given type declaration *)
    : Env.env * Export.env =     (* Returns table of kinds and export env *)

  match decl with 
    (* Linear term declaration *)
    | DteLinear (pat, t) -> 
      (* Infer type of term *)
      let lenv = Env.linearize env in
      let ty, (t_lenv, t_mod) = infer ktbl xenv env Error.dummy t in
      (* If judgement is strict and some variables haven't been used, error *)
      if t_mod = Strict && not (Env.subset lenv t_lenv) then 
        Typefail.unused xenv (locate Error.dummy t) (Env.pick (Env.difference t_lenv lenv));    
      (* Typecheck the pattern and get the bindings to add *)
      let bindings = pattern ktbl xenv lenv Error.dummy pat ty in
      (* Add them to the environment *)
      let env = Env.binds env bindings in
      let xenv =
        (* Try to bind *)
        try Bindings.fold (fun (x, _) xenv -> Export.bind xenv x) bindings xenv
        with
          (* Already used *)
          | Bound (x, _) -> Typefail.bound xenv (locate_atom x) x
          (* We bound a top. *)
          | NoBindTop x -> Typefail.no_bind_top xenv (locate_atom x) "be bound"
      in
      (* Return *) 
      env, xenv

(* ------------------------------------------------------------------------------- *)
(* Typechecking a declaration *)

(* Handle declarations *)
and declare ktbl xenv env = function 
  (* Term declaration *)
  | DeclTerm d ->  
      (* Bind term declaration *)
      let env, xenv = term_declaration ktbl xenv env d in 
      (* Return *)
      ktbl, xenv, env
  (* Type declaration *)  
  | DeclType (d, md_tycon, md_ktbl) -> 
      (* Bind type declaration *)
      let tycon, ktbl, xenv = type_declaration ktbl xenv d in 
      (* Save kinds table metadata & type metadata *)
      md_ktbl := Some ktbl;
      md_tycon := Some tycon;
      (* Return *)
      ktbl, xenv, env
  (* Toplevel term *)
  | DeclTop (t, md_ty) -> 
      (* FIXME: MODALITY ? WTF GIRL *)
      (* Infer type of toplevel type *)
      let ty, (lenv, _) = infer ktbl xenv env Error.dummy t in
      (* Recompose env, since exponentials never will be consumed *)
      let env = Env.compose lenv env in 
      (* Save type metadata *)
      md_ty := Some ty;
      (* Return *)
      ktbl, xenv, env

(* ------------------------------------------------------------------------------- *)
(* Typechecking a program or a declaration *)

(* Handle declaration *)
let declaration xenv env ktable decl = 
  let ktbl, xenv, env = declare ktable xenv env decl in 
  (Terms.petrify_decl decl), ktbl, xenv, env

(* A complete program is typechecked within empty environments. *)
let program ((Program (decls, metadata_ktbl) as p) : pre_program) =
  (* Fold over definition *)
  let ktable, xenv, env = 
    List.fold_left
      (fun (ktable, xenv, env) decl -> 
        (* Try declaration *)
        let ktable, xenv, env = 
          try declare ktable xenv env decl
          with 
            | NoInfer handler -> handler (); Error.fail ()
        in ktable, xenv, env)
        (Kinds.empty, Export.empty, Env.empty)
        decls
  in 
  (* Save metadata *)
  metadata_ktbl := Some ktable;
  (* Return *)
  (Terms.petrify p), xenv, env

(* ------------------------------------------------------------------------------- *)
(* Metadata *)

let rec type_of (term: fterm) = match term with 
  (* Constants *)
  | TeConst (TeOne) -> TyOne
  | TeConst (TeTop) -> TyTop 
  (* Variable *)
  | TeVar (_, ty) -> ty
  (* Dty con *)
  | TeData (_, ty) -> ty
  (* Constructs *)
  | TeSimPair (t, t') -> TyTensor (type_of t, type_of t')
  | TeAltPair (t, t') -> TyWith (type_of t, type_of t')
  | TeUnionLeft (t, ty) -> TyPlus (type_of t, ty)
  | TeUnionRight (ty, t) -> TyPlus (ty, type_of t)
  | TeBang (_, ty) -> ty
  | TeZero (_, ty) -> ty
  (* Applications *)
  | TeLinAbs (_, dom, body) -> TyLollipop (dom, type_of body)
  | TeApp (_, _, metadata) -> metadata.codomain
  | TeTyApp (_, t, info) -> fill info.context t
  | TeTyAbs (a, term) -> TyForall (abstract a (type_of term))
  | TeGive (_, _, body) -> type_of body
  (* Destructors *)
  | TeMatch (_, _, _, ty) -> ty
  (* Annotations *)
  | TeTyAnnot (_, ty) -> ty
  | TeLoc (_, t) -> type_of t
