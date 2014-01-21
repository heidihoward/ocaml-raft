open Core.Std
open Common

(** [Env] contains modules for handling state including PrueState and StateList
 * *)
module StateHandler :
      sig

      type state

       (** [t] holds the statehandler, this contains all the state for the
        * nodes as will as managing simulated failures *)
        type t
        val find : t -> IntID.t -> state status

        val find_wst : t -> IntID.t -> state

        (** [add sl id state] If there isn't already a state associated with
         * id when [add] will add state as a live node to statehandler. If it
         * already exists and its live, it also updates it and keeps the node
         * status. If you try to update  *)
        val add : t -> IntID.t -> state -> t
        val from_listassoc : (IntID.t *  state status) list -> t
        val init : int -> t

        val check_condition : t -> f:( (IntID.t * state status) -> bool) -> bool
        
        (** [kill] and [wake] are used to simulate nodes being killed and
         * recovering *)
        val kill : t -> IntID.t -> MonoTime.t -> t
        val wake : t -> IntID.t -> MonoTime.t -> t

        (** [leader_agreed] returns true if the majority of nodes are up and all
         * agree on term and leader *)
        val leader_agreed: t -> bool
      end
