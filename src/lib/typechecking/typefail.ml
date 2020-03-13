open Printf
open Env
open Lang.Printer
open Utils

(* ------------------------------------------------------------------------------- *)
(* Linear environment errors *)

let unbound xenv loc x =
  Error.error [loc] (sprintf
    "Unbound identifier %s."
    (print_atom xenv x)
  )

let bound xenv loc x =
  Error.error [loc] (sprintf
    "Binding %s would erase previous unconsumed linear variable.\n"
    (print_atom xenv x)
  )

let no_more xenv loc x =
  Error.error [loc] (sprintf
    "Linear variable %s has already been fully consumed.\n"
    (print_atom xenv x)
  )

let unused xenv loc (x, uses) = 
  Error.error [loc] (sprintf
    "Linear variable %s is unused (%d uses left).\n"
    (print_atom xenv x)
    uses
  )

let no_linear xenv loc kind x = 
  Error.error [loc] (sprintf
    "This term manipulates the linear variable %s.\nA %s cannot manipulate free linear variables.\n"
    (print_atom xenv x)
    kind
  )

let no_discard_linear xenv loc ty = 
  Error.error [loc] (sprintf
    "Linear variable of type %s cannot be discarded.\n"
    (print_type xenv ty)
  )
  
(* ------------------------------------------------------------------------------- *)
(* Additive errors *)

let no_uniform_branch_times xenv loc1 loc2 x use1 use2 = 
  Error.error [loc1; loc2] (sprintf
      "Linear variable %s is consumed %d times in a branch, and %d times in another.\n"
      (print_atom xenv x)
      use1
      use2
    )
  
let no_uniform_branch_type xenv loc1 loc2 x t1 t2 = 
  Error.error [loc1; loc2] (sprintf
      "Linear variable %s is defined as type %s in a branch, and type %s in another.\n"
      (print_atom xenv x)
      (print_type xenv t1)
      (print_type xenv t2)
    )

let no_uniform_branch_unbound ?(linear=true) xenv loc x = 
  let variable = if linear then "Linear variable" else "Variable" in
  Error.error [loc] (sprintf
      "%s %s was not bound in all branches.\n"
      variable
      (print_atom xenv x)
    )

let mismatch_additive xenv loc1 loc2 x = function
  (* Check reason *)
  | DifferentType (ty, ty') -> no_uniform_branch_type xenv loc1 loc2 x ty ty'
  | DifferentMult (0, _) -> no_uniform_branch_unbound xenv loc1 x
  | DifferentMult (_, 0) -> no_uniform_branch_unbound xenv loc2 x
  | DifferentMult (m, m') -> no_uniform_branch_times xenv loc1 loc2 x m m'

(* ------------------------------------------------------------------------------- *)
(* Elimination (match&let) errors *)

let nonlinear_pattern xenv loc x = 
  Error.error [loc] (sprintf
    "%s was already bound : pattern must be linear\n"
    (print_atom xenv x)
  )

let no_bind_top xenv loc kind = 
  Error.error [loc] (sprintf
    "Values of type %s can never %s.\n"
    (print_type xenv Lang.Types.TyTop)
    kind
  )

let missing_clause xenv loc ty =
  Error.error [loc] (sprintf
    "Pattern matching over %s is not exhaustive missing.\n"
    (print_type xenv ty)
  )

(* ------------------------------------------------------------------------------- *)
(* Inference errors *)

let cant_infer_refute _xenv loc = 
  Error.error [loc] (sprintf
    "Couldn't infer type of refutation. Use 'refute [type] with [term]' instead.\n"
  )

let cant_infer_match _xenv loc = 
  Error.error [loc] (sprintf
    "Couldn't infer return type of match. Use 'match [term] return [type] with' instead, or add type annotations to some clause.\n"
  )

(* ------------------------------------------------------------------------------- *)
(* Type declaration errors *)

let dtycon_bound xenv ktbl dty =
  let ty, _ = Lang.Kinds.find_tycon ktbl dty in
  Error.errora [dty] (sprintf
    "The data constructor %s is already defined in type %s.\n"
    (print_atom xenv dty) (print_atom xenv ty)
  )

let tycon_bound xenv tycon =
  Error.errora [tycon] (sprintf
    "The type constructor %s is already defined.\n"
    (print_atom xenv tycon)
  )
  
let tycon_arity_mismatch xenv x expected got =
  Error.errora [x] (sprintf
    "The type constructor %s expects %d type arguments,\nbut is applied to %d type arguments.\n"
    (print_atom xenv x) expected got
  )

let invalid_type_scheme ?(kind="data constructor signature") xenv dtycon ty =
  Error.errora [dtycon] (sprintf
    "Type %s is not a valid type scheme (in data constructor %s).\nIt cannot be used as a %s.\n"
    (print_type xenv ty) (print_atom xenv dtycon) kind
  )


let invalid_type_scheme_tycon xenv dtycon expected got =
  Error.errora [dtycon] (sprintf
    "Type scheme for data constructor %s must return a value of type %s.\nGot %s instead."
    (print_atom xenv dtycon)
    (print_atom xenv expected)
    (print_atom xenv got)
  )

(* ------------------------------------------------------------------------------- *)
(* Generic type errors *)

let unbound_id_kind x kind =
  Error.errora [x] (sprintf
    "Unbound %s %s."
    kind (Identifier.name (Atom.identifier x))
  )

let mismatch xenv loc expected inferred =
  Error.error [loc] (sprintf
    "Type mismatch.\nExpected: %s\nInferred: %s\n"
    (print_type xenv expected)
    (print_type xenv inferred)
  )

let expected_form xenv loc form ty =
  Error.error [loc] (sprintf
    "Type mismatch: expected %s.\nInferred: %s\n"
    form
    (print_type xenv ty)
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
