open Core.Std
open Common
open MonoTime
module Mach = Statemach.KeyValStr

        type t = private {
          (** Generic state as specified by the protocol *) 
          term : Index.t;
          mode : role;
          votedFor : IntID.t option;
          log : Mach.cmd Log.t;
          lastlogIndex : Index.t;
          lastlogTerm : Index.t;
          commitIndex : Index.t;
          votesFailed : IntID.t list;
          votesGranted : IntID.t list;
          nextIndex : (IntID.t * Index.t) list;
          matchIndex : (IntID.t * Index.t) list;
          (** Simulation specfic state, need removing/altering for real
           * implementation *)
          time : MonoTime.t;
          timer : int; 
          (** this flag is used to indicate if event of a timer
          has happened since last checked, a better method for this should be
          used *)
          id : IntID.t;
          allNodes : IntID.t list;
          leader : IntID.t option;
          state_mach : Mach.t;
          outstanding_request : (Index.t * Rpcs.ClientArg.t) option;
          safety_monitor : RaftMonitorWrapper.t;
          backoff :int ;
          possible_leader: bool;
        } 

        (** [statecall] are created to modify state *)
        type statecall =
            IncrementTerm
          | Reset
          | Set
          | Vote of IntID.t
          | StepDown of Index.t
          | VoteFrom of IntID.t * bool
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
          | ReplicationFailure of IntID.t * Index.t
          | ReplicationSuccess of IntID.t * Index.t
          | AddClientRequest of Index.t * Rpcs.ClientArg.t
          | RemoveClientRes 


        
        (** [init] id (list of other ids) will create the state *)
        val init : IntID.t -> IntID.t list -> bool -> t
        (** [print] turns state into string for debugging *)
        val print : t -> string
        (** [tick] is the only way to modify state *)
        val tick : statecall -> t -> t