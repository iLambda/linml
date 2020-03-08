open PPrint
open Types
open Utils

let line            = break 1
let softline        = group line

let zero = string "0"
let one = string "1"
let top = string "T"
let arrow = string " ->"
let lolli = string " -o"
let wth = string " & "
let plus = string " + "
let tensor = string " * "
let bang = string "!"
let forall = string "forall"


(* Print an atom *)
let pvar env x =
  (* Resolve the atom, get its name, and return it *)
  string (Identifier.name (Export.resolve env x))

(* Types. *)

(* In order to provide proper parenthesization, the structure of the
   printing functions reflects that of the parser: there are several
   levels of priority. *)
let rec pty0 env ty =
  match ty with
    (* No print bound var *)
    | TyBoundVar _ -> assert false
    (* Constants *)
    | TyZero -> zero 
    | TyOne -> one 
    | TyTop -> top  
    (* a *)
    | TyFreeVar a -> pvar env a
    (* a! *)
    | TyBang ty -> pty0 env ty ^^ bang
    (* a * b *)
    | TyTensor (ty1, ty2) -> pty0 env ty1 ^^ tensor ^^ pty0 env ty2
    (* a & b *)
    | TyWith (ty1, ty2) -> pty0 env ty1 ^^ wth ^^ pty0 env ty2
    (* (a) *)
    | _ -> parens (pty env ty)

and pty1 env ty =
  match ty with
    (* A + B *)
    | TyPlus (ty1, ty2) -> pty1 env ty1 ^^ plus ^^ pty1 env ty2
    (* t *)
    | _ -> pty0 env ty

and pty env ty =
  group (
    match ty with
      (* A -o B *)
      | TyLollipop (domain, codomain) ->
          pty1 env domain ^^
          lolli ^^ softline ^^
          pty env codomain
      (* A -> B *)
      | TyArrow (domain, codomain) ->
          pty1 env domain ^^
          arrow ^^ softline ^^
          pty env codomain
      (* forall *)
      | TyForall _ -> pforall env [] ty
      (* t *)
      | _ -> pty1 env ty
  )

and pforall env qs = function
  | TyForall body ->
      (* Make a new atom to name the forall bound variable *)
      let a = Atom.fresh (Types.hint body) in
      (* Bind it *)
      let env = Export.bind env a in
      (* Call recursively *)
      pforall env (a :: qs) (fill body (TyFreeVar a))
  | ty ->
      (* Finally print *)
      nest 2 (
        forall ^^
        concat_map (fun a -> space ^^ pvar env a) (List.rev qs) ^^ dot ^^ line ^^
        pty env ty
      )


(* The all-purpose buffer *)
let buffer = Buffer.create 2048

(* Helper to render document *)
let doc2string doc =
  Buffer.clear buffer;
  PPrint.ToBuffer.pretty 0.95 78 buffer doc;
  Buffer.contents buffer

(* Print an atom. *)
let print_atom env atom = doc2string (pvar env atom)

(* Print a type. *)
let print_type env ty = doc2string (pty env ty)

(* Print a program. *)
let print_program _program = ""
