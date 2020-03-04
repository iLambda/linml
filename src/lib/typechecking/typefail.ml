open Printf
open Env
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

let no_linear xenv loc kind x = 
  Error.error [loc] (sprintf
    "This term manipulates the linear variable %s.\nA %s cannot manipulate free linear variables."
    (print_atom xenv x)
    kind
  )

let no_uniform_branch_times xenv loc1 loc2 x use1 use2 = 
  Error.error [loc1; loc2] (sprintf
      "Linear variable %s is consumed %d times in a branch, and %d times in another."
      (print_atom xenv x)
      use1
      use2
    )
  
let no_uniform_branch_type xenv loc1 loc2 x t1 t2 = 
  Error.error [loc1; loc2] (sprintf
      "Linear variable %s is defined as type %s in a branch, and type %s in another."
      (print_atom xenv x)
      (print_type xenv t1)
      (print_type xenv t2)
    )

let no_uniform_branch_unbound ?(linear=true) xenv loc x = 
  let variable = if linear then "Linear variable" else "Variable" in
  Error.error [loc] (sprintf
      "%s %s was not bound in all branches."
      variable
      (print_atom xenv x)
    )

let mismatch_additive xenv loc1 loc2 x = function
  (* Check reason *)
  | DifferentType (ty, ty') -> no_uniform_branch_type xenv loc1 loc2 x ty ty'
  | DifferentMult (0, _) -> no_uniform_branch_unbound xenv loc1 x
  | DifferentMult (_, 0) -> no_uniform_branch_unbound xenv loc2 x
  | DifferentMult (m, m') -> no_uniform_branch_times xenv loc1 loc2 x m m'

let nonlinear_pattern xenv loc x = 
  Error.error [loc] (sprintf
    "%s was already bound : pattern must be linear"
    (print_atom xenv x)
  )

let no_discard_linear xenv loc ty = 
  Error.error [loc] (sprintf
    "Linear variable of type %s cannot be discarded."
    (print_type xenv ty)
  )

let mismatch xenv loc expected inferred =
  Error.error [loc] (sprintf
    "Type mismatch.\nExpected: %s\nInferred: %s"
    (print_type xenv expected)
    (print_type xenv inferred)
  )

let expected_form xenv loc form ty =
  Error.error [loc] (sprintf
    "Type mismatch: expected %s.\nInferred: %s\n"
    form
    (print_type xenv ty)
  )

let no_bind_top xenv loc kind = 
  Error.error [loc] (sprintf
    "Values of type %s can never %s."
    (print_type xenv Lang.Types.TyTop)
    kind
  )

let form_mismatch loc form1 form2 = 
  Error.error [loc] (sprintf
    "Type mismatch: expected %s, got %s instead.\n"
    form1
    form2
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
