(** [Des] modules is the main body of the Raft simulator *)

module RaftSim :
  functor (Id : Common.NODE_ID) ->
  functor (MonoTime : Clock.TIME) ->
  functor (Entry : Common.ENTRY) ->
  functor (L : Common.LOG) ->
  functor (P : Common.PARAMETERS) ->
  sig
    val debug : string -> unit
    val start_time : MonoTime.t
    val timeout : Common.role -> MonoTime.span
    val nxt_failure : MonoTime.t -> MonoTime.t
    val nxt_reset : MonoTime.t -> MonoTime.t
    module Comms :
    sig
      val unicast :
        Id.t ->
        MonoTime.t ->
        (MonoTime.t, Id.t, 'a) Common.Event.event ->
        (MonoTime.t, Id.t, 'a) Common.Event.t
      val broadcast :
        Id.t list ->
        MonoTime.t ->
        (MonoTime.t, Id.t, 'a) Common.Event.event ->
        (MonoTime.t, Id.t, 'a) Common.Event.t list
    end
    val checkElection : State.t -> bool
    type checker =
        Follower_Timeout of Common.Index.t
      | Candidate_Timeout of Common.Index.t
      | Leader_Timeout of Common.Index.t
    val startCand :
      State.t ->
      State.t * (MonoTime.t, Id.t, State.t) Common.Event.t list
    val checkTimer :
      checker -> (MonoTime.t, Id.t, State.t) Common.Event.event
    val dispatchHeartbeat :
      State.t ->
      State.t * (MonoTime.t, Id.t, State.t) Common.Event.t list
    val startFollow :
      Common.Index.t ->
      State.t ->
      State.t * (MonoTime.t, Id.t, State.t) Common.Event.t list
    val startLeader :
      State.t ->
      State.t * (MonoTime.t, Id.t, State.t) Common.Event.t list
    val stepDown :
      Common.Index.t ->
      State.t ->
      State.t * (MonoTime.t, Id.t, State.t) Common.Event.t list
    val requestVoteRq :
      Common.Index.t ->
      Id.t ->
      Common.Index.t ->
      Common.Index.t ->
      (MonoTime.t, Id.t, State.t) Common.Event.event
    val requestVoteRs :
      Common.Index.t ->
      bool ->
      Id.t -> (MonoTime.t, Id.t, State.t) Common.Event.event
    val heartbeatRq :
      Common.Index.t ->
      Id.t -> (MonoTime.t, Id.t, State.t) Common.Event.event
    val heartbeatRs :
      Common.Index.t ->
      (MonoTime.t, Id.t, State.t) Common.Event.event
    val finished : StateList.t -> bool
    val printline : string
    val get_time_span : StateList.t -> string
    val wake :
      State.t ->
      State.t * (MonoTime.t, Id.t, State.t) Common.Event.t list
    val kill :
      State.t ->
      State.t * (MonoTime.t, Id.t, 'a) Common.Event.t list
    val run_multi :
      term:MonoTime.t ->
      StateList.t ->
      (MonoTime.t, Id.t, State.t) Common.EventList.t -> unit
    val init_eventlist :
      int -> (MonoTime.t, Id.t, State.t) Common.Event.t list
    val start : unit -> unit
  end
