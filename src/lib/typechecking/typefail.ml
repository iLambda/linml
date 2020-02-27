open Printf
open Lang.Printer
open Utils

let unbound xenv loc x =
  Error.error [loc] (sprintf
    "Unbound identifier %s."
    (print_atom xenv x)
  )

let bound xenv loc x =
  Error.error [loc] (sprintf
    "Binding %s would erase previous unconsumed linear variable."
    (print_atom xenv x)
  )

let no_more xenv loc x =
  Error.error [loc] (sprintf
    "Linear variable %s has already been fully consumed."
    (print_atom xenv x)
  )

let unused xenv loc (x, uses) = 
  Error.error [loc] (sprintf
    "Linear variable %s is unused (%d uses left)."
    (print_atom xenv x)
    uses
  )

let bang_no_consume xenv loc x = 
  Error.error [loc] (sprintf
    "This term consumes the linear variable %s.\n
     A term that manipulates linear variables cannot be banged."
    (print_atom xenv x)
  )


let no_uniform_branch_times xenv loc x use1 use2 = 
  Error.error [loc] (sprintf
      "Linear variable %s is consumed %d times in a branch, and %d times in another."
      (print_atom xenv x)
      use1
      use2
    )
  
let no_uniform_branch_type xenv loc x t1 t2 = 
  Error.error [loc] (sprintf
      "Linear variable %s is defined as type %s in a branch, and type %s in another."
      (print_atom xenv x)
      (print_type xenv t1)
      (print_type xenv t2)
    )

let no_uniform_branch_unbound xenv loc x = 
  Error.error [loc] (sprintf
      "Linear variable %s was not bound in all branches."
      (print_atom xenv x)
    )

let mismatch_env _xenv _loc _env1 _env2 =
  (* Digest checker *)
  failwith "TODO"
  (* let checker env env' = 
    AtomMap.iter 
      (fun x b -> 
        match b, AtomMap.find_opt x env' with 
          (* Missing from digest. *)
          | _, None -> no_uniform_branch_unbound xenv loc x
          (* Different types *)
          | BindLin (ty, _), Some (BindLin(ty, _)) when not (Types.equal ty ty') -> 
            no_uniform_branch_type xenv loc x ty ty'
          (* Same types, but different consumption multiplicity *)
          | Some (_, m') when m <> m' -> 
            no_uniform_branch_times xenv loc x m m'
          (* Nothing *)
          | _ -> ())
      env
  in
  (* Apply *)
  checker env1 env2;
  checker env2 env1 *)

let mismatch xenv loc expected inferred =
  Error.error [loc] (sprintf
    "Type mismatch.\nExpected: %s\nInferred: %s"
    (print_type xenv expected)
    (print_type xenv inferred)
  )

let expected_form xenv loc form ty =
  Error.error [loc] (sprintf
    "Type mismatch: I expected %s type.\nInferred: %s\n"
    form
    (print_type xenv ty)
  )

let arity_mismatch xenv loc kind1 x kind2 expected found =
  Error.error [loc] (sprintf
    "The %s %s expects %d %s arguments,\nbut is applied to %d %s arguments.\n"
    kind1 (print_atom xenv x) expected kind2 found kind2
  )

let missing_clause xenv loc ty =
  Error.error [loc] (sprintf
    "Pattern matching over %s is not exhaustive missing.\n"
    (print_type xenv ty)
  )
