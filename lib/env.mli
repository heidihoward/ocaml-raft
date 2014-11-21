open Core.Std
open Common
open MonoTime
module Mach = Statemach.KeyValStr
open State


(** [Env] contains modules for handling state including PrueState and StateList
 * *)


(** [State] holds the state of one of the nodes in the Raft simulation, the
 * state is accessable via type t with is a private record, its only modifiable
 * via buy passing statecalls to the tick function *)

       (** [t] holds the statehandler, this contains all the state.t for the
        * nodes as will as managing simulated failures *)
        type t
        val find : t -> IntID.t -> State.t status

        val find_wst : t -> IntID.t -> State.t

        (** [add sl id state] If there isn't already a state.t associated with
         * id when [add] will add state as a live node to statehandler. If it
         * already exists and its live, it also updates it and keeps the node
         * status. If you try to update  *)
        val add : t -> IntID.t -> State.t -> t
(*        val from_listassoc : (IntID.t *  State.t status) list -> t *)
        val init : int -> bool -> int -> t

        val check_condition : t -> f:( (IntID.t * State.t status) -> bool) -> bool
        
        (** [kill] and [wake] are used to simulate nodes being killed and
         * recovering *)
        val kill : t -> IntID.t -> MonoTime.t -> t
        val wake : t -> IntID.t -> MonoTime.t -> t

        (** [leader_agreed] returns true if the majority of nodes are up and all
         * agree on term and leader *)
        val leader_agreed: t -> bool

        val check_safety: t -> string

        val get_leader: t -> State.t option

