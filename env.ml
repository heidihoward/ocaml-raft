open Core.Std
open Common


module PureState  = 
  functor (MonoTime: Clock.TIME) ->
  functor (Mach: Statemach.MACHINE) -> struct

  (* Split this record down into sections, seperating general statem *)
        type t = {
          (** Generic state as specified by the protocol *) 
          term : Index.t;
          mode : role;
          votedFor : IntID.t option;
          log : Mach.cmd ListLog.t;
          lastlogIndex : Index.t;
          lastlogTerm : Index.t;
          lastApplied : Index.t;
          votesResponded : IntID.t list;
          votesGranted : IntID.t list;
          nextIndex : Index.t;
          lastAgreeIndex : Index.t;
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
   | Commit of Mach.cmd


  let init me all =
    { term = Index.init();
      mode = Follower;
      time = MonoTime.init;
      timer = false;
      votedFor = None;
      log = ListLog.init(); 
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
      state_mach = Mach.init()
    } 

  let refresh s:t =
    { term = s.term;
      mode = Follower;
      time = s.time ;
      timer = false;
      votedFor = None;
      log = s.log; 
      lastlogIndex = Index.init();
      lastlogTerm = Index.init();
      lastApplied = Index.init();
      votesResponded = [];
      votesGranted = [];
      nextIndex = Index.init();
      lastAgreeIndex = Index.init(); 
      id = s.id; 
      allNodes = s.allNodes;
      leader = None;
      state_mach = s.state_mach
    } 


  let id_print = function  None -> "none" | Some x -> IntID.to_string x

  let print s = 
    "-------------------------------------------------------\n"^
    " | Time: "^(MonoTime.to_string (s.time()))^
    " | ID: "^(IntID.to_string s.id)^
    " | Term: "^(Index.to_string s.term)^
    " | Mode: "^(string_of_role s.mode)^"\n"^
    " | VotedFor: "^(IntID.to_string s.id)^
    " | All Nodes: "^(List.to_string ~f:IntID.to_string s.allNodes)^
    " | Votes Recieved: "^ (List.to_string ~f:IntID.to_string s.votesGranted)^
    " | Leader: "^(string_of_option (IntID.to_string) s.leader)^"\n"^
    " | State Machine: "^(Mach.to_string s.state_mach)^
    " | Replicated Log: "^(ListLog.to_string s.log ~f:(Mach.cmd_to_string))^
    "\n-------------------------------------------------------"
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
    | StepDown tm ->
        { s with mode=Follower;
        term = tm;
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
        refresh s
    | Commit x ->
        { s with
        state_mach = Mach.commit s.state_mach x}

end

module StateHandler =
  functor (MonoTime: Clock.TIME) ->
  functor (Mach: Statemach.MACHINE ) -> struct

module State = PureState(MonoTime)(Mach)

  type t = (IntID.t,State.t status) List.Assoc.t

  let find sl id  = match (List.Assoc.find sl id) with
    | Some x -> x | None -> Notfound

  (* returns a list of state.t for all live nodes, useful for iterating over *)
  let get_live sl = 
    let f (_,s) = (match s with
    | Live _ -> true 
    | Down _ | Notfound -> false ) in
    let live_list = List.filter sl ~f in
    let g (_,s) = (match s with Live st -> st) in
    List.map live_list ~f:g

  let leader_agreed sl = 
    let (live: State.t list) = get_live sl in
    (* check majority of nodes are live *)
    if ((List.length live)*2 > (List.length sl)) then
      match live with | hd::_ ->
      match hd.leader with 
      | None -> false  (* if hd doesn't know leader *)
      | Some _ ->
          (* check all live nodes agree on leader and term *)
      List.for_all live ~f:(fun s -> s.leader=hd.leader && s.term=hd.term)
    else false
  
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
