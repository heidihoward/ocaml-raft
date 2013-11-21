open Core.Std

module MonoTime = struct
  type t = int
  let init () = 0
  let diff t1 t2 = abs(t1-t2)
  let comp t1 t2 = phys_equal t1 t2
  let iter t = t+1
  let add t1 t2 = t1+t2
  let create x = x
  let print t = string_of_int t
end

module ID = struct
  type t = int
  let from_int x = x
  let to_int x  = x
  let comp t1 t2 = phys_equal t1 t2
  let print x = string_of_int x
end

module LogEntry = struct
  type t = A | B | C
end

module Log = struct
  let init () = []
  type t = LogEntry.t list
  let append t x = t::x

end

module State = struct

  type role = Follower | Candidate | Leader
  
  type t = 
    { term : MonoTime.t;
      mode: role;
      time: MonoTime.t;
      heartbeat: bool; (*if there's been a heartbeat since last check *)
      votedFor: ID.t option;
      log: Log.t;
      lastlogIndex: MonoTime.t;
      lastlogTerm: MonoTime.t;
      lastApplied: MonoTime.t;
      votesResponded: ID.t list;
      votesGranted: ID.t list;
      nextIndex: MonoTime.t;
      lastAgreeIndex: MonoTime.t;
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


  let init =
    { term = MonoTime.init();
      mode = Follower;
      time = MonoTime.init();
      heartbeat = false;
      votedFor = None;
      log = Log.init(); 
      lastlogIndex = MonoTime.init();
      lastlogTerm = MonoTime.init();
      lastApplied = MonoTime.init();
      votesResponded = [];
      votesGranted = [];
      nextIndex = MonoTime.init();
      lastAgreeIndex = MonoTime.init(); 
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
    | IncrementTerm-> 
        let t = MonoTime.iter s.time in
       { s with time=t }
    | Vote id -> 
        { s with votedFor = Some id}



end

let timeout = MonoTime.create 5

(*state is the global information, readable by all and only modified by
 * statecalls via State.tick, the mutablity here should be handled better *)
let state = ref State.init

let debug_active = ref true

let debug x = if !debug_active then 
  let time = MonoTime.print !state.time in
  (printf "[%s] %s \n" time x)

(* wrappers around ugly mutable state *)
let update tk =  state := State.tick !state tk

(* -- all the events, can probably be moved into own module --*)

(* type event =  E: unit -> (MonoTime.t * event) list *)
type 'a e = Next of ('a * (unit ->  'a e)) list

let rec incrTime () = update IncrementTime; Next []

and startCand () = debug "Entering Candidate Mode";
  update IncrementTerm;
  update (Vote !state.id); 
  let reqs = List.map !state.allNodes 
    ~f:(fun rcv -> (!state.time, requestVoteRq !state.term !state.id !state.lastlogIndex
    !state.lastlogTerm rcv)) in
  let t = MonoTime.add !state.time timeout in
  Next ((t, checkTimer)::reqs)

and checkTimer () = debug "Checking heartbeat timer"; 
  if (!state.mode = Follower) then
    (* if heartbeat is true, we have rec a packet in the last election timeout*)
    if !state.heartbeat then 
      let t = MonoTime.add !state.time timeout in
      update Reset;
      Next [(t, checkTimer )] 
    (* we have timedout so become candidate *)
    else Next [(!state.time, startCand)]
  else Next []

and startFollow () = debug "Entering Follower mode";
  let t = MonoTime.add !state.time timeout in
  Next [(t, checkTimer)]

and requestVoteRq term cand_id lst_index last_term rvc () =
  debug ("Dispatch request to"^ ID.print rvc );
  Next []

  (*
let RequestVoteRs

let AppendEntriesRq

let AppendEntriesRs
*)

let eventlist = Next [(MonoTime.init(), startFollow);
                 (MonoTime.create 30, incrTime)]

let rec run = function 
  | Next events -> 
  let f (t,_) = MonoTime.comp !state.time t in 
  match (List.partition_tf events ~f) with
  | ([],[]) -> debug "finished all events"
    (* if no more events for this time then increment time and try again *)
  | ([],_) ->  ignore(incrTime ()); run (Next events)
    (* if there are event to be executed, run them *)
  | (now,later) -> 
      let results = List.map now ~f:(fun (t,e) -> match e () with Next x -> x) in
    (* will return any new events to add to event list so add to list*)
  run (Next (later@(List.concat results)))

let main = 
  run(eventlist)


  
