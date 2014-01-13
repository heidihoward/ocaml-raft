open Core.Std
open Common


module PureState  = 
  functor (MonoTime: Clock.TIME) ->
  functor (Log: LOG) -> struct

  (* Split this record down into sections, seperating general statem *)
  type t = 
    { term : Index.t;
      mode: role;
      time: (unit -> MonoTime.t);
      timer: bool; (*if there's been a heartbeat since last check *)
      votedFor: IntID.t option;
      log: Log.t;
      lastlogIndex: Index.t;
      lastlogTerm: Index.t;
      lastApplied: Index.t;
      votesResponded: IntID.t list;
      votesGranted: IntID.t list;
      nextIndex: Index.t;
      lastAgreeIndex: Index.t;
      id: IntID.t;
      allNodes: IntID.t list; 
      leader: IntID.t option
    } with sexp
   
  type statecall = 
   | IncrementTerm
   | Reset | Set
   | Vote of IntID.t
   | StepDown of Index.t
   | VoteFrom of IntID.t
   | StartCandidate
   | StartLeader
   | SetTime of MonoTime.t
   | SetLeader of IntID.t
   | SetTerm of Index.t
   | Restart


  let init me all =
    { term = Index.init();
      mode = Follower;
      time = MonoTime.init;
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

  let empty () =
    { term = Index.init();
      mode = Follower;
      time = MonoTime.init;
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
      id = (IntID.from_int 0); (*this is a bad hack *)
      allNodes = [];
      leader = None;
    } 


  let id_print = function  None -> "none" | Some x -> IntID.to_string x

  let print s = 
    " id: "^(IntID.to_string s.id)^
    " | Term: "^(Index.to_string s.term)^
    " | Mode: "^(string_of_role s.mode)^
    " | Time: "^(MonoTime.to_string (s.time()))^"\n"^
    " | VotedFor: "^(IntID.to_string s.id)^
    " | All Nodes: "^(List.to_string ~f:IntID.to_string s.allNodes)^
    " | Votes Recieved: "^ (List.to_string ~f:IntID.to_string s.votesGranted)^
    " | Leader: "^(id_print s.leader)^
     "\n---------------------------------------------------\n"
 (* sexp_of_t s |> Sexp.to_string *)



  let tick tk s =
  match tk with
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
        { s with time=(MonoTime.store t)}
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
        assert (t >= s.term);
        { s with term = t;
          votedFor = None }
    | Restart -> 
        { empty() with 
          time = s.time;
          votedFor = s.votedFor;
          term = s.term}

end

module StateHandler =
  functor (MonoTime: Clock.TIME) ->
  functor (L: LOG) -> struct

module State = PureState(MonoTime)(L)

  type t = (IntID.t,State.t status) List.Assoc.t

  let find sl id  = match (List.Assoc.find sl id) with
    | Some x -> x | None -> Notfound

  let add sl id state = List.Assoc.add sl id state 
  let from_listassoc x = x

  let init n : t =
    let id_list = List.init n ~f:(IntID.from_int) in
    let remove x xs = List.filter xs ~f:(fun y -> not (x = y)) in 
    let gen_state id id_list = State.init id (remove id id_list) in
    List.map 
    ~f:(fun node_id -> node_id , (Live (gen_state node_id id_list))) id_list

  let check_condition sl ~f = 
    match (List.find sl ~f) with Some _ -> true | None -> false 

  let kill sl id = 
    match (find sl id) with
    | Live s -> List.Assoc.add sl id (Down s)

  let wake sl id =
    match (find sl id) with 
    | Down s -> List.Assoc.add sl id (Live s)

end
