open Lang.Types
open Utils.Atom

(* Errors thrown when an operation is invalid *)
exception Unbound of atom 
exception NoMore of atom 
exception Bound of atom * int
exception Incompatible

(** The type of environment *)
type env

(** The type of digests *)
type digest = (ftype * int) AtomMap.t

(** [empty] represents the empty environment *)
val empty: env

(** [is_empty env] returns true iff the environment only has exponential bindings *)
val is_empty: env -> bool

(** [compose envl enve] returns a new environment containing the linear bindings of [env]
 and the exponential bindins of [enve] *)
val compose: env -> env -> env

(** [pick env] picks a linear variable from the environment *)
val pick: env -> atom * int

(** [bind env x ty] binds a to [x] the type [ty] in the environment *)
val bind: env -> atom -> ftype -> env

(** [retrieve env x] consumes a binding from the environment *)
val retrieve: env -> atom -> env * ftype

(** [union env env'] merges the two environments *)
val union: env -> env -> env

(** [inter env env'] returns the intersection of the environments *)
val inter: env -> env -> env

(** [equal env env'] checks if two environments are equal *)
val equal: env -> env -> bool

(** [disjoint env env'] returns true iff the environments are disjoint *)
val disjoint: env -> env -> bool

(** [consumed env env'] returns the linear variables that were consumed 
    when going from [env] to [env']*)
val consumed: env -> env -> env