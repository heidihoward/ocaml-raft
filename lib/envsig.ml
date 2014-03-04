open Core.Std
open Common

(** [Env] contains modules for handling state including PrueState and StateList
 * *)
module type STATEHANDLER =
  functor (MonoTime : Clock.TIME) ->
    functor (Mach : Statemach.MACHINE) ->
      sig

(** [State] holds the state of one of the nodes in the Raft simulation, the
 * state is accessable via type t with is a private record, its only modifiable
 * via buy passing statecalls to the tick function *)
module State :
      sig

        type t = private {
          (** Generic state as specified by the protocol *) 
          term : Index.t;
          mode : role;
          votedFor : IntID.t option;
          log : (Index.t * Index.t * Mach.cmd) list;
          lastlogIndex : Index.t;
          lastlogTerm : Index.t;
          commitIndex : Index.t;
          votesResponded : IntID.t list;
          votesGranted : IntID.t list;
          nextIndex : (IntID.t * Index.t) list;
          matchIndex : (IntID.t * Index.t) list;
          (** Simulation specfic state, need removing/altering for real
           * implementation *)
          time : unit -> MonoTime.t;
          timer : bool; 
          (** this flag is used to indicate if event of a timer
          has happened since last checked, a better method for this should be
          used *)
          id : IntID.t;
          allNodes : IntID.t list;
          leader : IntID.t option;
          state_mach : Mach.t;
        } with sexp
        
        (** [statecall] are created to modify state *)
        type statecall =
            IncrementTerm
          | Reset
          | Set
          | Vote of IntID.t
          | StepDown of Index.t
          | VoteFrom of IntID.t
          | StartCandidate
          | StartLeader
          | SetTime of MonoTime.t
          | SetLeader of IntID.t
          | SetTerm of Index.t
          | Restart
          | Commit of Index.t
          | AppendEntries of (Index.t * Index.t * Mach.cmd) list
          | AppendEntry of (Index.t * Index.t * Mach.cmd)
           | RemoveEntries of Index.t * Index.t


        
        (** [init] id (list of other ids) will create the state *)
        val init : IntID.t -> IntID.t list -> t
        (** [print] turns state into string for debugging *)
        val print : t -> string
        (** [tick] is the only way to modify state *)
        val tick : statecall -> t -> t
      end

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
        val init : int -> t

        val check_condition : t -> f:( (IntID.t * State.t status) -> bool) -> bool
        
        (** [kill] and [wake] are used to simulate nodes being killed and
         * recovering *)
        val kill : t -> IntID.t -> MonoTime.t -> t
        val wake : t -> IntID.t -> MonoTime.t -> t

        (** [leader_agreed] returns true if the majority of nodes are up and all
         * agree on term and leader *)
        val leader_agreed: t -> bool

        val check_safety: t -> unit
      end
