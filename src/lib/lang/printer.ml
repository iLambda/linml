open PPrint
open Terms
open Types
open Utils

(* Line break helpers *)
let line            = break 1
let softline        = group line

(* Keywords *)
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
let ty = string "type "

(* Helpers *)
let spacing f2 t2s = 
  if List.compare_length_with t2s 0 = 0 
  then space 
  else !^" " ^^ separate !^" " (List.map f2 t2s) ^^ !^" "

(* Buffer *)
let buffer = Buffer.create 2048

(* Print an atom *)
let pvar env x =
  (* Resolve the atom, get its name, and return it *)
  ignore (Export.resolve env x);
  string (Identifier.name (Atom.identifier x))
  (* string (string_of_int (Atom.uid x)) *)
  (* string (Identifier.name (Export.resolve env x)) *)

(* Types. *)

(* In order to provide proper parenthesization, the structure of the
   printing functions reflects that of the parser: there are several
   levels of priority. *)
let rec ptym1 env ty =
  match ty with 
    (* Constants *)
    | TyZero -> zero 
    | TyOne -> one 
    | TyTop -> top  
    (* a *)
    | TyFreeVar a -> pvar env a
    (* a *)
    | TyCon (x, []) -> pvar env x
    (* a! *)
    | TyBang ty -> pty0 env ty ^^ bang
    (* (a) *)
    | _ -> parens (pty env ty)

and pty0 env ty =
  match ty with
    (* No print bound var *)
    | TyBoundVar _ -> assert false
    (* a t1 ... tn *)
    | TyCon (x, ((_::_) as tys)) -> (pvar env x) ^^ space ^^ separate !^" " (List.map (pty0 env) tys)
    (* a * b *)
    | TyTensor (ty1, ty2) -> pty0 env ty1 ^^ tensor ^^ pty0 env ty2
    (* a & b *)
    | TyWith (ty1, ty2) -> pty0 env ty1 ^^ wth ^^ pty0 env ty2
    (* t *)
    | _ -> ptym1 env ty

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

and ptyvar env x = brackets (pvar env x)

and ptycon env = function 
  | DtyGADT (tycon, tyvars, ctors) -> 
      (* Print 'type ' *)
      ty ^^ 
      (* Print tycon name *)
      pvar env tycon ^^ 
      (* Print '[a] ... [a] = '*)
      (spacing (ptyvar env) tyvars) ^^ string "=" ^^ softline ^^
      (* Print | dtycon -> ty *)
      separate !^" | " 
        (List.map
          (fun (x, ty) -> pvar env x ^^ !^ " : " ^^ pty env ty) ctors)
        
(* Helper to render document *)
let doc2string doc =
  Buffer.clear buffer;
  PPrint.ToBuffer.pretty 0.95 78 buffer doc;
  Buffer.contents buffer

(* Print an atom. *)
let print_atom env atom = doc2string (pvar env atom)

(* Print a type. *)
let print_type env ty = doc2string (pty env ty)

(* Prints a type constructor declaration *)
let print_tydecl env tydecl = doc2string (ptycon env tydecl)

(* Print a program. *)
let print_program _program = ""
