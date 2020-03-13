open Types
open Utils
open Utils.Atom

(* ------------------------------------------------------------------------------- *)
(* Types *)

(* Errors *)
exception Dtycon_bound of atom
exception Dtycon_unbound of atom
exception Invalid_type_scheme of atom * ftype
exception Invalid_type_scheme_con of { dtycon: atom; expected: atom; got: atom }
exception Tycon_arity of { tycon: atom; expected:int; got: int }
exception Tycon_bound of atom
exception Tycon_unbound of atom
(* exception Tyvar_bound of atom *)
exception Tyvar_unbound of atom

(* Table containing all the kinds (tyctor, datactor) *)
type env = {
  (* The type constructors : maps tycon names to their definition *)
  ty_cons: type_ctor AtomMap.t;
  (* The data constructors : maps dtycon names to their tycon *)
  dty_cons: atom AtomMap.t;
  (* The free type variables *)
  free_tyvars: AtomSet.t
}

(* A data ctor *)
and data_ctor = 
  | Constructor of
      atom *          (* The name of the constructor *) 
      ftype           (* The type of the constructor *)

(* A type ctor *)
and type_ctor =
  | TyconGADT of 
      atom list *       (* Type variables *)
      data_ctor list    (* Constructors *) 
      
  
(* ------------------------------------------------------------------------------- *)
(* Constants *)

(* An empty datatype table *)
let empty : env = { 
  ty_cons=AtomMap.empty; 
  dty_cons=AtomMap.empty; 
  free_tyvars=AtomSet.empty;
}

(* ------------------------------------------------------------------------------- *)
(* Free variables *)

(** [fv ktbl x] returns true iff the free type variable [x] is in ktbl *)
let fv { free_tyvars; _ } x = 
  AtomSet.mem x free_tyvars

(** [fv_add ktbl x] adds the free type variable [x] in ktbl *)
let fv_add { ty_cons; dty_cons; free_tyvars } x = 
  (* Bind *)
  let free_tyvars =  AtomSet.add x free_tyvars in
  (* Return *)
  { ty_cons; dty_cons; free_tyvars }
  
(** [fv_remove ktbl x] removes the free type variable [x] in ktbl *)
let fv_remove { ty_cons; dty_cons; free_tyvars } x = 
  (* Bind *)
  let free_tyvars =  AtomSet.remove x free_tyvars in
  (* Return *)
  { ty_cons; dty_cons; free_tyvars }
  
(** [fv_removes ktbl xs] removes the free type variables [x] in ktbl *)
let fv_removes ktbl xs = 
  List.fold_left fv_remove ktbl xs

(** [fv_adds ktbl xs] adds the free type variable [x] in ktbl *)
let fv_adds ktbl xs = 
  List.fold_left fv_add ktbl xs

(* ------------------------------------------------------------------------------- *)
(* Type constructors *)

(** [has_tycon ktbl x] check if type [x] is defined in the kinds table.  *)
let has_tycon ktbl x = AtomMap.mem x ktbl.ty_cons

(** [has_dtycon ktbl x] check if data constructor [x] is defined in the kinds table.  *)
let has_dtycon ktbl x = AtomMap.mem x ktbl.dty_cons

(** [arity_tycon ktbl x] returns the arity of type constructor [x]. 
    Raises [Tycon_unbound x] if [x] isn't a valid ty con. *)
let arity_tycon ({ ty_cons; _ } as ktbl) x = 
  (* If not in, error *)
  if not (has_tycon ktbl x) then 
    raise (Tycon_unbound x);
  (* Return arity *)
  match AtomMap.find x ty_cons with 
    | TyconGADT (xs, _) -> List.length xs 

(* ------------------------------------------------------------------------------- *)
(* Type well formedness *)

(** [well_formed ktbl t] ensures type is well formed. *)
let rec well_formed ktbl = function 
  (* Base cases *)
  | TyOne | TyTop | TyZero -> ()
  | TyBoundVar _ -> assert false
  (* Free variable *)
  | TyFreeVar x when fv ktbl x -> ()
  | TyFreeVar x -> raise (Tyvar_unbound x)
  (* Recursive *)
  | TyPlus (t1, t2) | TyTensor (t1, t2) 
  | TyWith (t1, t2) | TyLollipop (t1, t2) 
  | TyArrow (t1, t2) -> well_formed ktbl t1; well_formed ktbl t2
  | TyBang t -> well_formed ktbl t
  (* Type constructor *)
  | TyCon (x, tys) -> 
      (* Check if types are well formed *)
      List.iter (well_formed ktbl) tys;
      (* Check if type constructor is in ktbl *)
      if not (has_tycon ktbl x) then 
        raise (Tycon_unbound x); 
      (* Check if arity is respected *)
      if List.compare_length_with tys (arity_tycon ktbl x) <> 0 then 
        raise (Tycon_arity { 
          tycon=x;
          expected=arity_tycon ktbl x; 
          got=List.length tys})
        
  (* Forall. Instantiate and check *)
  | TyForall ctx -> 
      (* Fill *)
      let x = Atom.fresh (Types.hint ctx) in 
      let ty = Types.fill ctx (TyFreeVar x) in
      (* Add freevar to ktbl *)
      let ktbl = fv_add ktbl x in
      (* Check if resulting type is well formed *)
      well_formed ktbl ty


(* ------------------------------------------------------------------------------- *)
(* Helpers *)

let valid_type_scheme ktbl tyvars tycon dtycon t = 
  (* Remove all head foralls *)
  let rec remove_foralls = function
    (* A forall *)
    | TyForall ctx ->   
        (* Make an atom and fill the context, then remove on this *)
        let v = Utils.Atom.fresh (Types.hint ctx) in 
        let t = Types.fill ctx (TyFreeVar (v)) in
        remove_foralls t
    (* Return unaduntered *)
    | ty -> ty
    
  (* Remove all head arrows *)
  and remove_arrows = function 
    (* An arrow *)
    | (TyLollipop (_, ty) | TyArrow (_, ty))-> remove_arrows ty 
    (* Return unadultered *)
    | ty -> ty
    
  (* Check if constructor is valid *)
  and check_con = function 
    (* The constructor *)
    | TyCon (x, tyargs) -> 
      (* Check if name is the same *)
      if not (Atom.equal x tycon) then 
        raise (Invalid_type_scheme_con { dtycon; expected=tycon; got=x });
      (* Check if arity is ok *)
      if List.compare_lengths tyvars tyargs <> 0 then 
        raise (Tycon_arity { 
          tycon=x; 
          expected=(List.length tyvars); 
          got=(List.length tyargs) })  
    (* Not a constructor. Error *)
    | _ -> raise (Invalid_type_scheme (dtycon, t))
  in 
  (* Check if type is well formed *)
  well_formed ktbl t;  
  (* Check if type scheme is valid *)
  check_con (remove_arrows (remove_foralls t))
  
(* ------------------------------------------------------------------------------- *)
(* Datatype table manipulation *)

(* Register a new tycon *)
let register_tycon ktbl x = function 
  (* A GADT *)
  | TyconGADT (tyvars, ctors) as d -> 
      (* Check if tycon exists *)
      if has_tycon ktbl x then 
        raise (Tycon_bound x);
      (* Add type variables in env *)
      let ktbl_inside = fv_adds ktbl tyvars in
      let ktbl_inside =  { 
        ty_cons=AtomMap.add x d ktbl_inside.ty_cons; 
        dty_cons=ktbl_inside.dty_cons; 
        free_tyvars=ktbl_inside.free_tyvars 
      } in
      (* Check if dtycons exist *)
      let dtycons_names = 
        List.map
        (fun (Constructor (y, _)) -> 
          if has_dtycon ktbl y then 
            raise (Dtycon_bound x);
          y)
        ctors
      in
      (* Check if type scheme is valid *)
      List.iter
        (fun (Constructor (dtycon, ty)) -> 
          valid_type_scheme ktbl_inside tyvars x dtycon ty)
      ctors;      
      (* Register all the data type constructors *)
      let dty_cons = 
        List.fold_left 
          (fun dct dty -> AtomMap.add dty x dct) ktbl.dty_cons dtycons_names 
      in 
      let ty_cons = AtomMap.add x d ktbl.ty_cons  in 
      (* Return new kinds table *)
      { ty_cons=ty_cons; 
        dty_cons=dty_cons; 
        free_tyvars=ktbl.free_tyvars }

(** [find_tycon ktbl x] returns the name and tycon that the dtycon [x] belongs to *)
let find_tycon { ty_cons; dty_cons; _ } x = 
  let tycon_name = AtomMap.find x dty_cons in 
  let tycon = AtomMap.find tycon_name ty_cons in 
  tycon_name, tycon

(** [lookup_tycon ktbl x] returns the type constructor [x] *)
let lookup_tycon { ty_cons; _ } x = 
  AtomMap.find x ty_cons