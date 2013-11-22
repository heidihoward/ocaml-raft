open Core.Std

module MonoTime = Clock.FakeTime

module Index = struct
  type t = int with compare
  let succ = succ
  let init () = 0
end 
(*
module MonoTime = struct
  (*TODO split MonoTime into a seperate module for terms, indexs etc *)
  (*TODO define the sig *)
  type t = int
  let init () = 0
  let diff t1 t2 = abs(t1-t2)
  (* Implement comparitor operators *)
  let comp t1 t2 = phys_equal t1 t2
  let iter t = t+1
  let add t1 t2 = t1+t2
  let create x = x
  let print t = string_of_int t
end
*)
module ID = struct
  type t = int
  let from_int x = x
  let to_int x  = x
  let comp t1 t2 = phys_equal t1 t2
  let print x = string_of_int x
end

module LogEntry = struct
  (*TODO copy over proper application from other .mls *)
  type t = A | B | C
end

module Log = struct
  let init () = []
  type t = LogEntry.t list
  let append t x = t::x

end

module State = struct

  type role = Follower | Candidate | Leader
  
  (* Split this record down into sections, seperating general statem *)
  type t = 
    { term : Index.t;
      mode: role;
      time: MonoTime.t;
      heartbeat: bool; (*if there's been a heartbeat since last check *)
      votedFor: ID.t option;
      log: Log.t;
      lastlogIndex: Index.t;
      lastlogTerm: Index.t;
      lastApplied: Index.t;
      votesResponded: ID.t list;
      votesGranted: ID.t list;
      nextIndex: Index.t;
      lastAgreeIndex: Index.t;
      id: ID.t;
      allNodes: ID.t list; 
    }
   
  type statecall = 
(*    | Apply  (* now safe to apply LastAppled to the state machine*)
    | StepDown of MonoTime.t (* you are out of date, become follower in this new  term *)
    | StepUp *)
   | IncrementTime 
   | IncrementTerm
   | Reset 
   | Vote of ID.t
   | StepDown of Index.t
   | VoteFrom of ID.t


  let init =
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
      id = ID.from_int 1;
      allNodes = [ID.from_int 2; ID.from_int 3];
    } 

(*  let print s :state -> unit = *)

  let tick s tk =
  match tk with
    | IncrementTime -> 
        let t = MonoTime.iter s.time in
       { s with time=t }
    | Reset -> 
       {s with heartbeat=false}
    | IncrementTerm -> 
        let t = Index.succ s.time in
       { s with time=t }
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

let timeout = MonoTime.create 5

(*state is the global information, readable by all and only modified by
 * statecalls via State.tick, the mutablity here should be handled better *)

let debug_active = ref true

let debug x = if !debug_active then 
  (printf " %s \n"  x)

(* ----- all the events, can probably be moved into own module -----*)

(* type event =  E: unit -> (MonoTime.t * event) list *)
type 'a e = Next of ('a *(State.t -> State.t * 'a e)) list

type checker = Timeout | Vote (*what event triggered a check of electron outcome *)

let rec incrTime s = (State.tick s IncrementTime, Next [])

and startCand s = debug "Entering Candidate Mode";
  let snew = State.tick (State.tick s IncrementTerm) (Vote s.id ) in
  let reqs = List.map snew.allNodes 
    ~f:(fun rcv -> (snew.time, requestVoteRq snew.term snew.id snew.lastlogIndex
    snew.lastlogTerm rcv)) in
  let t = MonoTime.add snew.time timeout in
  (snew, Next ((t, checkElection Timeout)::reqs))

and checkTimer (s:State.t) = debug "Checking heartbeat timer"; 
  if (s.mode = Follower) then
    (* if heartbeat is true, we have rec a packet in the last election timeout*)
    if s.heartbeat then 
      let t = MonoTime.add s.time timeout in
      (State.tick s Reset, Next [(t, checkTimer )]) 
    (* we have timedout so become candidate *)
    else (s,Next [(s.time, startCand)])
  else (s,Next [])

and startFollow (s:State.t) = debug "Entering Follower mode";
  let t = MonoTime.add s.time timeout in
  (s,Next [(t, checkTimer)])

and requestVoteRq term cand_id lst_index last_term rvc s =
  debug ("Dispatch request to"^ ID.print rvc );
  (s,Next [])
  
and requestVoteRs term voteGranted id (s:State.t) = 
  if (term > s.term) 
  then (State.tick s (StepDown term) ,Next [(s.time,startFollow)]) 
  else if (voteGranted) 
  then (State.tick s (VoteFrom id), Next [(s.time,checkElection Timeout)])
  else (s, Next [])

and checkElection c s = 
  match c with 
  | Timeout -> (*TODO: check electon outcome *) (s,Next [])
  | Vote -> (* TODO:check election outcome *) (s,Next [])

(*
let AppendEntriesRq

let AppendEntriesRs
*)

let eventlist = Next [(MonoTime.init(), startFollow);
                 (MonoTime.create 30, incrTime)]

let rec run (s:State.t) = function 
  | Next events -> 
  let f (t,_) = MonoTime.comp s.time t in 
  match (List.partition_tf events ~f) with
  | ([],[]) -> debug "finished all events"
    (* if no more events for this time then increment time and try again *)
  | ([],_) ->  let (snew,_) = incrTime s in
     run snew (Next events)
    (* if there are event to be executed, run them *)
  | (next::thn,later) -> 
      let snew,enew = (match next with (_,e) -> e s) in
      let enew = (match enew with Next x -> x) in
    (* will return any new events to add to event list so add to list*)
  run snew (Next (enew @ thn @ later)) 

let main = run State.init eventlist 


  
