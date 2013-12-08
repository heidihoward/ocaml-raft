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
  val string_of_role: role -> string

  (*TODO: Ask anil about how a concrete type can be in the struct and sig
    * without copy/paste *)
  (* TODO Find a much better way to represent state for quick read access, i.e.
   * divide into different records for each state and one for log etc..*)
  type t = 
    { term : Index.t;
      mode: role;
      time: MonoTime.t;
      timer: bool; (*if there's condition filled since last check *)
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
      leader: Id.t option;
    }
 
  

  type statecall = 
(*    | Apply  (* now safe to apply LastAppled to the state machine*)
    | StepDown of MonoTime.t (* you are out of date, become follower in this new  term *)
    | StepUp *)
   | IncrementTime 
   | IncrementTerm
   | Reset | Set 
   | Vote of Id.t
   | StepDown of Index.t
   | VoteFrom of Id.t
   | StartCandidate
   | StartLeader
   | SetTime of MonoTime.t
   | SetLeader of Id.t
   | SetTerm of Index.t

  val init: Id.t -> Id.t list -> t
  val tick: statecall -> t -> t
  val print: t -> unit

end  

module PureState : STATE  = 
  functor (Id: ID) -> 
  functor (MonoTime: TIME) ->
  functor (Index: INDEX) -> 
  functor (Entry: ENTRY) ->
  functor (L: LOG) -> struct

  module Log = L(Entry)
  type role = Follower | Candidate | Leader

  let string_of_role = function
    | Follower -> "Follower"
    | Candidate -> "Candidate"
    | Leader -> "Leader"
  
  (* Split this record down into sections, seperating general statem *)
  type t = 
    { term : Index.t;
      mode: role;
      time: MonoTime.t;
      timer: bool; (*if there's been a heartbeat since last check *)
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
      leader: Id.t option
    }
   
  type statecall = 
(*    | Apply  (* now safe to apply LastAppled to the state machine*)
    | StepDown of MonoTime.t (* you are out of date, become follower in this new  term *)
    | StepUp *)
   | IncrementTime 
   | IncrementTerm
   | Reset | Set
   | Vote of Id.t
   | StepDown of Index.t
   | VoteFrom of Id.t
   | StartCandidate
   | StartLeader
   | SetTime of MonoTime.t
   | SetLeader of Id.t
   | SetTerm of Index.t


  let init me all =
    { term = Index.init();
      mode = Follower;
      time = MonoTime.init();
      timer = false;
      votedFor = None;
      log = Log.init(); 
      lastlogIndex = Index.init();
      lastlogTerm = Index.init();
      lastApplied = Index.init();
      votesResponded = [];
      votesGranted = [];
      nextIndex = Index.init();
      lastAgreeIndex = Index.init(); 
      id = me;
      allNodes = all;
      leader = None;
    } 

  let id_print = function  None -> "none" | Some x -> Id.to_string x

  let print s = 
    printf " ID: %s | Term: %s | Mode: %s | Time: %s |\n VotedFor: %s | " 
    (Id.to_string s.id)
    (Index.to_string s.term) 
    (string_of_role s.mode)
    (MonoTime.to_string s.time)
    (id_print s.votedFor);
    printf "All Nodes: %s | Votes Recieved: %s | Leader: %s \n" 
    (List.to_string ~f:Id.to_string s.allNodes)
    (List.to_string ~f:Id.to_string s.votesGranted)
    (id_print s.leader)

  

  let tick tk s =
  match tk with
    | IncrementTime -> 
        let t = MonoTime.succ s.time in
       { s with time=t }
    | Reset -> 
       {s with timer=false}
    | Set -> 
        {s with timer=true}
    | IncrementTerm -> 
        let t = Index.succ s.term in
       { s with term=t }
    | Vote id -> 
        { s with votedFor = Some id}
    | VoteFrom id ->
        { s with votesGranted = id::s.votesGranted }
    | StepDown term ->
        { s with mode=Follower;
        term = term;
        timer = false; 
        votedFor = None;
        votesResponded=[];
        votesGranted=[] }
    | StartCandidate -> 
        { s with mode=Candidate;
        timer = false;
        votedFor = Some s.id;
        votesResponded=[];
        votesGranted=[s.id];
        term = (Index.succ s.term)
        }
    | SetTime t -> 
        { s with time=t}
    | StartLeader -> 
        { s with mode=Leader;
        timer = false;
        votedFor = None;
        votesResponded=[];
        votesGranted=[];
        leader = Some s.id
        }
    | SetLeader ld ->
        { s with leader= Some ld}
    | SetTerm t ->
        { s with term = t;
          votedFor = None }



end
