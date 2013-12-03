open Core.Std
open Common
open Clock

module type STATE = 
  functor (Id: ID) -> 
  functor (MonoTime: TIME) ->
  functor (Index: INDEX) -> 
  functor (E: ENTRY) ->
  functor (Log:  LOG ) -> sig


  type role = Follower | Candidate | Leader
  type t = 
    { term : Index.t;
      mode: role;
      time: MonoTime.t;
      heartbeat: bool; (*if there's been a heartbeat since last check *)
      votedFor: Id.t option;
      log: Log(E).t; (*TODO Ask anil about how to do this properly *)
      lastlogIndex: Index.t;
      lastlogTerm: Index.t;
      lastApplied: Index.t;
      votesResponded: Id.t list;
      votesGranted: Id.t list;
      nextIndex: Index.t;
      lastAgreeIndex: Index.t;
      id: Id.t;
      allNodes: Id.t list; 
    }
 
  

  type statecall = 
(*    | Apply  (* now safe to apply LastAppled to the state machine*)
    | StepDown of MonoTime.t (* you are out of date, become follower in this new  term *)
    | StepUp *)
   | IncrementTime 
   | IncrementTerm
   | Reset 
   | Vote of Id.t
   | StepDown of Index.t
   | VoteFrom of Id.t

  val init: unit -> t
  val tick: t -> statecall -> t

end  

module PureState : STATE  = 
  functor (Id: ID) -> 
  functor (MonoTime: TIME) ->
  functor (Index: INDEX) -> 
  functor (Entry: ENTRY) ->
  functor (L: LOG) -> struct

  module Log = L(Entry)
  type role = Follower | Candidate | Leader
  
  (* Split this record down into sections, seperating general statem *)
  type t = 
    { term : Index.t;
      mode: role;
      time: MonoTime.t;
      heartbeat: bool; (*if there's been a heartbeat since last check *)
      votedFor: Id.t option;
      log: Log.t;
      lastlogIndex: Index.t;
      lastlogTerm: Index.t;
      lastApplied: Index.t;
      votesResponded: Id.t list;
      votesGranted: Id.t list;
      nextIndex: Index.t;
      lastAgreeIndex: Index.t;
      id: Id.t;
      allNodes: Id.t list; 
    }
   
  type statecall = 
(*    | Apply  (* now safe to apply LastAppled to the state machine*)
    | StepDown of MonoTime.t (* you are out of date, become follower in this new  term *)
    | StepUp *)
   | IncrementTime 
   | IncrementTerm
   | Reset 
   | Vote of Id.t
   | StepDown of Index.t
   | VoteFrom of Id.t


  let init () =
    { term = Index.init();
      mode = Follower;
      time = MonoTime.init();
      heartbeat = false;
      votedFor = None;
      log = Log.init(); 
      lastlogIndex = Index.init();
      lastlogTerm = Index.init();
      lastApplied = Index.init();
      votesResponded = [];
      votesGranted = [];
      nextIndex = Index.init();
      lastAgreeIndex = Index.init(); 
      id = Id.from_int 1;
      allNodes = [Id.from_int 2; Id.from_int 3];
    } 

(*  let print s :state -> unit = *)

  let tick s tk =
  match tk with
    | IncrementTime -> 
        let t = MonoTime.succ s.time in
       { s with time=t }
    | Reset -> 
       {s with heartbeat=false}
    | IncrementTerm -> 
        let t = Index.succ s.term in
       { s with term=t }
    | Vote id -> 
        { s with votedFor = Some id}
    | VoteFrom id ->
        { s with votesGranted = id::s.votesGranted }
    | StepDown term ->
        { s with mode=Follower; 
        heartbeat = false; 
        votedFor = None;
        votesResponded=[];
        votesGranted=[] }



end
