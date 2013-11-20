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
  let compare t1 t2= (t1==t2) 
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
    { currentTerm : MonoTime.t;
      mode: role;
      time: MonoTime.t;
      votedFor: ID.t option;
      log: Log.t;
      lastApplied: MonoTime.t;
      votesResponded: ID.t list;
      votesGranted: ID.t list;
      nextIndex: MonoTime.t;
      lastAgreeIndex: MonoTime.t }
   
  type statecall = 
(*    | Apply  (* now safe to apply LastAppled to the state machine*)
    | StepDown of MonoTime.t (* you are out of date, become follower in this new  term *)
    | StepUp *)
  IncrementTime 


  let init =
    { currentTerm = MonoTime.init();
      mode = Follower;
      time = MonoTime.init();
      votedFor = None;
      log = Log.init(); 
      lastApplied = MonoTime.init();
      votesResponded = [];
      votesGranted = [];
      nextIndex = MonoTime.init();
      lastAgreeIndex = MonoTime.init() } 

(*  let print s :state -> unit = *)

  let tick s tk =
  match tk with
    | IncrementTime -> let t = MonoTime.iter s.time in
    printf "%s" (MonoTime.print t);
    { s with time=t }

end

let timeout = MonoTime.create 5

(*state is the global information, readable by all and only modified by
 * statecalls via State.tick, the mutablity here should be handled better *)
let state = ref State.init

(* wrappers around ugly mutable state *)
let update tk =  state := State.tick !state tk

(* -- all the events, can probably be moved into own module --*)

type event =  E: unit -> (MonoTime.t * event) list 

let incrTime () = update IncrementTime; []
let checkTimer () = printf "hello"; []

let startFollow () = 
  let t = MonoTime.add !state.time timeout in
  [(t, checkTimer)]


(*
let RequestVoteRq term cand_id lst_index last_term =
  if (term 

let RequestVoteRs

let AppendEntriesRq

let AppendEntriesRs
*)

let eventlist = [startFollow]

let rec run events = 
  let f (t,_) = MonoTime.comp !state.time t in 
  match (List.partition_tf events ~f) with
    (* if no more events for this time then increment time and try again *)
  | ([],_) -> ignore(incrTime ()); run events
    (* if there are event to be executed, run them *)
  | (now,later) -> let results = List.map now ~f:(fun (t,e) -> e ()) in
    (* will return any new events to add to event list so add to list*)
  run later@(List.concat results)

let main = 
  run(eventlist)


  
