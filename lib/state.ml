open Core.Std
open Common
open MonoTime
module RaftMonitor = RaftMonitorWrapper
module Mach = Statemach.KeyValStr


(* Split this record down into sections, seperating general statem *)
    type t = {
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
      (* This timer is used in all 3 states and has different meanings depending on state *)
      timer : int; 
      (** this flag is used to indicate if event of a timer
      has happened since last checked, a better method for this should be
      used *)
      id : IntID.t;
      allNodes : IntID.t list;
      leader : IntID.t option;
      state_mach : Mach.t;
      outstanding_request : (Index.t * Rpcs.ClientArg.t) option;
      safety_monitor : RaftMonitor.t;
      backoff: int;
      possible_leader: bool;
    }




type statecall = 
| IncrementTerm
| Reset | Set
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


let init me all leader =
{ term = Index.init();
  mode = Follower;
  time = MonoTime.init;
  timer = 0;
  votedFor = None;
  log = Log.init(); 
  lastlogIndex = Index.init();
  lastlogTerm = Index.init();
  commitIndex = Index.init();
  votesFailed = [];
  votesGranted = [];
  nextIndex  = [];
  matchIndex = [];
  id = me;
  allNodes = all;
  leader = None;
  state_mach = Mach.init();
  outstanding_request = None;
  safety_monitor = RaftMonitor.tick (RaftMonitor.init()) `Startup ;
  backoff = 0;
  possible_leader = leader;
} 

let refresh s:t =
{ term = s.term;
  mode = Follower;
  time = s.time ;
  timer = s.timer;
  votedFor = s.votedFor;
  log = s.log; 
  lastlogIndex = s.lastlogIndex;
  lastlogTerm = s.lastlogTerm;
  commitIndex = s.commitIndex;
  votesFailed = [];
  votesGranted = [];
  nextIndex  = [];
  matchIndex = [];
  id = s.id; 
  allNodes = s.allNodes;
  leader = None;
  state_mach = s.state_mach;
  outstanding_request = None;
  safety_monitor =  RaftMonitor.tick (RaftMonitor.init()) `Recover;
  backoff = 0;
  possible_leader = s.possible_leader;
} 


let id_print = function  None -> "none" | Some x -> IntID.to_string x
let id_index_print (id,index) = (" ID: "^(IntID.to_string id)^"Index: "^(Index.to_string index) )

let print s = 
"-- NODE STATE ------------------------------------------------------------------------\n"^
" | Time: "^(MonoTime.to_string s.time)^
" | ID: "^(IntID.to_string s.id)^
" | Term: "^(Index.to_string s.term)^
" | Mode: "^(string_of_role s.mode)^"\n"^
" | VotedFor: "^(string_of_option IntID.to_string s.votedFor)^
" | All Nodes: "^(List.to_string ~f:IntID.to_string s.allNodes)^
" | Votes Unsuccessful: "^ (List.to_string ~f:IntID.to_string s.votesFailed)^    
" | Votes Successful: "^ (List.to_string ~f:IntID.to_string s.votesGranted)^
" | Backoff Number: "^(Int.to_string s.backoff)^
" | Leader: "^(string_of_option (IntID.to_string) s.leader)^"\n"^
" | State Machine: "^(Mach.to_string s.state_mach)^
" | Match Index: "^(List.to_string s.matchIndex ~f:id_index_print)^"\n"^
" | Next Index: "^(List.to_string s.matchIndex ~f:id_index_print)^"\n"^
" | Last Log Index: "^(Index.to_string s.lastlogIndex)^
" | Last Log Term: "^(Index.to_string s.lastlogTerm)^
" | Commit Index: "^(Index.to_string s.commitIndex)^
" | Replicated Log: "^(Log.to_string ~cmd_to_string:Mach.cmd_to_string s.log)^
" | Outstanding Request: "^(string_of_option (fun (id,_) -> Index.to_string id) s.outstanding_request)^
"\n-------------------------------------------------------------------------------------"
(* sexp_of_t s |> Sexp.to_string *)

(* let rec log_add original new_list = 
(*combine logs, if two items with same index, give proirity to new *)
match new_list with
| (index,term,cmd)::rest -> log_add ((index,term,cmd)::(List.filter original ~f:(fun (i,_,_) -> not(index=i)))) rest
| [] -> original *)


let update_commitIndex match_list commitIndex log curr_term =
let magority = (((List.length match_list +1) / 2)-1) in
let discard (_,index) = 
  (if index > commitIndex then Some index else None) in
let new_list = 
  List.sort ~cmp:(fun x y -> -1* (Index.compare x y))
    (List.filter_map ~f:discard match_list) in
(* new_list is now a list of indexes which are greater then current commit index *)
(* nth starts counting at 0 *)
(*  printf "new_list %s nth %s " (List.to_string ~f:Index.to_string new_list) (Int.to_string magority); *)
match List.nth new_list magority with 
| Some index -> 
  let (_,term) = Log.specific_index_term index log in
  if (curr_term=term) then index else commitIndex
| None -> commitIndex

let tick tk s =
match tk with
| Set -> 
    {s with timer = Int.succ (s.timer)}
| IncrementTerm -> 
    let t = Index.succ s.term in
   { s with term=t }
| Vote id -> 
     assert(s.votedFor = None);
     assert(s.mode=Follower);
    { s with votedFor = Some id}
| VoteFrom (id,success) -> (
  if success then 
    { s with votesGranted = id::s.votesGranted }
  else
    {s with votesFailed = id::s.votesFailed} )
| StepDown tm ->
    { s with mode=Follower;
    term = tm;
    votedFor = None;
    votesFailed=[];
    votesGranted=[];
    nextIndex  = [];
    matchIndex = []; 
    leader = None;
    backoff = 0;
    outstanding_request = None;
    safety_monitor = (
        match s.mode with 
        | Candidate -> RaftMonitor.tick s.safety_monitor `StepDown_from_Candidate
        | Leader -> RaftMonitor.tick s.safety_monitor `StepDown_from_Leader 
        | Follower -> s.safety_monitor );
    }
| StartCandidate -> 
    { s with mode=Candidate;
    backoff = if (List.length s.votesFailed * 2 >= List.length s.allNodes) then s.backoff +1 else s.backoff;
    votedFor = Some s.id;
    votesFailed=[];
    votesGranted=[s.id];
    term = (Index.succ s.term);
    outstanding_request = None;
    leader = None;
    safety_monitor =
        match s.mode with
        | Candidate -> RaftMonitor.tick s.safety_monitor `RestartElection
        | Follower -> RaftMonitor.tick s.safety_monitor `StartElection;
    }
| SetTime t -> 
    { s with time=t}
| StartLeader -> 
    { s with mode=Leader;
    votedFor = None;
    (* hack for debug *)
   (* votesResponded=[]; *)
   (* votesGranted=[]; *)
    leader = Some s.id;
    backoff = 0;
    outstanding_request = None;
    nextIndex  = List.map s.allNodes ~f:(fun id -> (id,Index.succ (s.lastlogIndex)) );
    matchIndex = List.map s.allNodes ~f:(fun id -> (id,Index.init()) );
    safety_monitor = RaftMonitor.tick s.safety_monitor `WinElection;
    }
| SetLeader ld ->
    { s with leader= Some ld}
| SetTerm t ->
    assert (t >= s.term);
    { s with term = t;
      votedFor = None;
      leader = None; }
| Restart -> 
    refresh s
(* RAFT SPEC: If leaderCommit > commitIndex, set commitIndex = min(leaderCommit, last log index) *)
(*TODO: once this works, put all log list stuff into its own module *)
| Commit new_index -> (
    let new_index = 
      (if (new_index < s.lastlogIndex) then new_index else s.lastlogIndex) in
    let new_mach = 
    (* TODO: cache response *)
      Mach.commit_many s.state_mach (Log.to_commit s.commitIndex new_index s.log) in
    { s with state_mach = new_mach; commitIndex=new_index} )
| AppendEntries entries -> 
    assert (s.mode=Follower);
    let last_term,last_index = (
      match entries with 
      | [] -> s.lastlogTerm, s.lastlogIndex
      | (i,t,_)::_ -> t,i ) in 
    let new_log = Log.appends entries s.log in
    { s with log = new_log; lastlogTerm=last_term; lastlogIndex=last_index; }
| AppendEntry (index, term, cmd) -> 
    assert (s.mode=Leader);
   {s with log = Log.append (index,term,cmd) s.log ; lastlogTerm=term; lastlogIndex=index}
| RemoveEntries (index,term) ->
   let new_log = Log.cut_entries index s.log in
   {s with log = new_log; lastlogTerm=term; lastlogIndex=index}
| ReplicationFailure (id,index_tried) -> (
    assert(s.mode=Leader);
   match (List.Assoc.find s.nextIndex id) with 
   | Some index -> 
      if (index=index_tried) then
        { s with nextIndex = (List.Assoc.add s.nextIndex id (Index.pred index)  )}
       else s
   | None -> assert false )
| ReplicationSuccess (id,index_new) -> (
    assert(s.mode=Leader);
    match (List.Assoc.find s.nextIndex id),(List.Assoc.find s.matchIndex id) with
    | Some next_index, Some match_index -> 
      let new_matchIndex = (
        if (match_index<index_new) then 
        List.Assoc.add s.matchIndex id index_new
        else s.matchIndex ) in
      let new_commitIndex = update_commitIndex new_matchIndex s.commitIndex s.log s.term in
        { s with 
        matchIndex = new_matchIndex ;
        nextIndex = (List.Assoc.add s.nextIndex id (Index.succ index_new));
        commitIndex = new_commitIndex ; 
        state_mach =  Mach.commit_many s.state_mach (Log.to_commit s.commitIndex new_commitIndex s.log)}
    | _ -> assert false )
| AddClientRequest (index,res) ->
  {s with outstanding_request = Some (index,res); }
| RemoveClientRes -> 
  { s with outstanding_request = None}