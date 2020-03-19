open Types
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
type env

(* A data ctor *)
type data_ctor = 
  | Constructor of
      atom *          (* The name of the constructor *) 
      ftype           (* The type of the constructor *)

(* A type *)
type type_ctor =
  | TyconGADT of 
      atom list *       (* Type variables *)
      data_ctor list    (* Constructors *) 

  
(* ------------------------------------------------------------------------------- *)
(* Constants *)

(* An empty kinds table *)
val empty : env

(* ------------------------------------------------------------------------------- *)
(* Free variables *)

(** [fv ktbl x] returns true iff the free type variable [x] is in ktbl *)
val fv: env -> atom -> bool

(** [fv_add ktbl x] adds the free type variable [x] in ktbl. *)
val fv_add: env -> atom -> env

(** [fv_adds ktbl xs] adds the free type variable [x] in ktbl.*)
val fv_adds: env -> atom list -> env

(** [fv_remove ktbl x] removes the free type variable [x] in ktbl *)
val fv_remove: env -> atom -> env

(** [fv_removes ktbl xs] removes the free type variables [x] in ktbl *)
val fv_removes: env -> atom list -> env

(* ------------------------------------------------------------------------------- *)
(* Type well formedness *)

(** [well_formed ktbl t] ensures type is well formed.
    Raises [Tyvar_unbound x] if [x] is an undeclared free variable in [t].
    Raises [Tycon_unbound x] if [x] is an unbound type variable in [t]. 
    Raises [Tycon_arity] if a tycon arity wasn't respected. *)
val well_formed: env -> ftype -> unit 

(* ------------------------------------------------------------------------------- *)
(* Type constructors *)

(** [has_tycon ktbl x] check if type [x] is defined in the kinds table.  *)
val has_tycon : env -> atom -> bool

(** [has_dtycon ktbl x] check if data constructor [x] is defined in the kinds table.  *)
val has_dtycon : env -> atom -> bool 

(** [find_tycon ktbl x] returns the name and tycon that the dtycon [x] belongs to.
    Raises [Dtycon_unbound x] if the dtycon [x] wasn't defined. *)
val find_tycon : env -> atom -> atom * type_ctor

(** [arity_tycon ktbl x] returns the arity of type constructor [x]. 
    Raises [Tycon_unbound x] if [x] isn't a valid ty con. *)
val arity_tycon : env -> atom -> int

(** [arity_dtycon ktbl x] returns the arity of data constructor [x]. 
    Raises [Dtycon_unbound x] if [x] isn't a valid dty con. *)
val arity_dtycon : env -> atom -> int

(** [register ktbl x d] registers a new datatype in the kinds table.
    Raises [Dtycon_bound y] if [y] is already defined as a data con.
    Raises [Tycon_bound x] if [x] is already defined as a ty con.
    Raises [Tycon_arity] if a some datacon has wrong arity.
    Raises [Tyvar_unbound x] if a tyvar [x] wasn't bound.
    Raises [Invalid_type_scheme] if a data con has an invalid type scheme.
    Raises [Invalid_type_scheme_con] if a data con has an invalid type scheme. *)
val register_tycon : env -> atom -> type_ctor -> env 

(** [lookup_tycon ktbl x] returns the type constructor [x].
    Raises [Tycon_unbound x] if [x] doesn't exists *)
val lookup_tycon: env -> atom -> type_ctor

(** [lookup_dtycon ktbl x] returns the type of dtycon [x].
    Raises [Dtycon_unbound x] if [x] doesn't exists *)
val lookup_dtycon: env -> atom -> ftype