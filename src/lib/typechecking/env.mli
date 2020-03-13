open Lang.Types
open Utils
open Utils.Atom

(* Errors thrown when an operation is invalid *)
exception Unbound of atom 
exception NoMore of atom 
exception Bound of atom * int
exception NoBindTop of atom

(** The type of linear environments *)
type linenv

(** The type of environments *)
type env

(** The type of sets of bindings *)
module Bindings : Set.S with type elt = (atom * ftype)

(** The reason for the separation *)
type separation =
  | DifferentMult of int * int
  | DifferentType of ftype * ftype

(* ------------------------------------------------------------------------------- *)
(* Constants *)

(** [empty] represents the empty environment *)
val empty: env

(** [nothing] represents the linear empty environment *)
val nothing: linenv

(* ------------------------------------------------------------------------------- *)
(* Environment manipulation *)

(** [consume env x] consumes a binding from the environment.
    Raises [Unbound] if [x] not present.
    Raises [NoMore x] if [x] is depleted.  *)
val consume: env -> atom -> env * ftype

(** [bind env x ty] binds [x] to the type [ty] in the environment.
    Raises [Bound (x, multiplicity)] if [x] was already bound.
    Raises [NoBindTop] if [ty] is top. *)
val bind: env -> atom -> ftype -> env

(** [binds env b] binds all the bindings [x] in the environment.
    Raises [Bound (x, multiplicity)] if [x] was already bound.
    Raises [NoBindTop] if [ty] is top. *)
val binds: env -> Bindings.t -> env

(* ------------------------------------------------------------------------------- *)
(* Environment conversion *)

(** [linearize env] returns a linear environment, containing only the linear variables of [env] *)
val linearize: env -> linenv

(** [exponentials env] returns an environment, containing only the exponential bindings of [env] *)
val exponentials: env -> env

(** [compose lenv env] returns a new environment containing the linear bindings of [lenv]
 and the exponential bindings of [env] *)
val compose: linenv -> env -> env

(** [of_bindings b] returns a new environment containing the linear bindings of [b].
    Raises [Bound (x, multiplicity)] if [x] was already bound.
    Raises [NoBindTop] if [ty] is top. *)
val of_bindings: Bindings.t -> linenv

(* ------------------------------------------------------------------------------- *)
(* Multiset operations *)

(** [is_void lenv] returns true iff the linear environment and the exponential env are empty *)
val is_void: env -> bool

(** [is_empty lenv] returns true iff the linear environment is empty *)
val is_empty: linenv -> bool

(** [has lenv x] checks if a variable belongs to a linear environment *)
val has: linenv -> atom -> bool

(** [multiplicity lenv x] checks the multiplicity of [x] in [lenv] *)
val multiplicity: linenv -> atom -> int

(** [equal lenv lenv'] checks if two linear environments are equal *)
val equal: linenv -> linenv -> bool

(** [subset lenv lenv'] returns true iff [env'] is a subset of [env] *)
val subset: linenv -> linenv -> bool

(** [difference lenv lenv'] returns the linear variables that are in [env] 
    but not in [env']*)
val difference: linenv -> linenv -> linenv
    
(** [inter lenv lenv'] returns the intersection of the environments*)
val inter: linenv -> linenv -> linenv

(** [split env x] returns two environments.
    The second one containing just the binding for [x] (or is empty if [x] is not in [env]),
    the first containing the rest of [env] *)
val split: linenv -> atom -> linenv * linenv

(** [merge env env'] merges two environments. *)
val merge: linenv -> linenv -> linenv

(** [purge env x] removes x from the environment altogether *)
val purge: linenv -> atom -> linenv

(** [purges env b] removes all the bindings [b] from the environment altogether *)
val purges: linenv -> Bindings.t -> linenv

(* ------------------------------------------------------------------------------- *)
(* Selection *)

(** [separate env env'] picks a binding that is either in [env] and not in [env'], or 
in [env'] but not in [env], and also returns the reason for the separation *)
val separate: linenv -> linenv -> atom * separation

(** [pick env] picks a linear variable from the environment *)
val pick: linenv -> atom * int

(* ------------------------------------------------------------------------------- *)
(* Helpers *)

(** [print xenv env] returns a string representing the linear environment *)
val print: ?color:bool -> env -> Export.env -> string

(** [print_top xenv env] returns a string representing the toplevel term of given type *)
val print_top: ?color:bool -> ftype -> Export.env -> string