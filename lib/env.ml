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
          log : Mach.cmd Log.t;
          lastlogIndex : Index.t;
          lastlogTerm : Index.t;
          commitIndex : Index.t;
          votesResponded : IntID.t list;
          votesGranted : IntID.t list;
          nextIndex : (IntID.t * Index.t) list;
          matchIndex : (IntID.t * Index.t) list;
          (** Simulation specfic state, need removing/altering for real
           * implementation *)
          time : unit -> MonoTime.t;
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
          seqNum : int ;
          safety_monitor : RaftMonitor.t
        }
   



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
   | Commit of Index.t
   | AppendEntries of (Index.t * Index.t * Mach.cmd) list
   | AppendEntry of (Index.t * Index.t * Mach.cmd)
   | RemoveEntries of Index.t * Index.t
   | ReplicationFailure of IntID.t * Index.t
   | ReplicationSuccess of IntID.t * Index.t
   | AddClientRequest of Index.t * Rpcs.ClientArg.t
   | RemoveClientRes 


  let init me all =
    { term = Index.init();
      mode = Follower;
      time = MonoTime.init;
      timer = 0;
      votedFor = None;
      log = Log.init(); 
      lastlogIndex = Index.init();
      lastlogTerm = Index.init();
      commitIndex = Index.init();
      votesResponded = [];
      votesGranted = [];
      nextIndex  = [];
      matchIndex = [];
      id = me;
      allNodes = all;
      leader = None;
      state_mach = Mach.init();
      outstanding_request = None;
      seqNum = 0;
      safety_monitor = RaftMonitor.init();
    } 

  let refresh s:t =
    { term = s.term;
      mode = Follower;
      time = s.time ;
      timer = s.timer;
      votedFor = None;
      log = s.log; 
      lastlogIndex = Index.init();
      lastlogTerm = Index.init();
      commitIndex = Index.init();
      votesResponded = [];
      votesGranted = [];
      nextIndex  = [];
      matchIndex = [];
      id = s.id; 
      allNodes = s.allNodes;
      leader = None;
      state_mach = s.state_mach;
      outstanding_request = None;
      seqNum = s.seqNum;
      safety_monitor = (* RaftMonitor.tick s.safety_monitor `Recover *) RaftMonitor.init() ;
    } 


  let id_print = function  None -> "none" | Some x -> IntID.to_string x
  let id_index_print (id,index) = (" ID: "^(IntID.to_string id)^"Index: "^(Index.to_string index) )

  let print s = 
    "-- NODE STATE ------------------------------------------------------------------------\n"^
    " | Time: "^(MonoTime.to_string (s.time()))^
    " | ID: "^(IntID.to_string s.id)^
    " | Term: "^(Index.to_string s.term)^
    " | Mode: "^(string_of_role s.mode)^"\n"^
    " | VotedFor: "^(IntID.to_string s.id)^
    " | All Nodes: "^(List.to_string ~f:IntID.to_string s.allNodes)^
    " | Votes Recieved: "^ (List.to_string ~f:IntID.to_string s.votesGranted)^
    " | Leader: "^(string_of_option (IntID.to_string) s.leader)^"\n"^
    " | State Machine: "^(Mach.to_string s.state_mach)^
    " | Match Index: "^(List.to_string s.matchIndex ~f:id_index_print)^"\n"^
    " | Next Index: "^(List.to_string s.matchIndex ~f:id_index_print)^"\n"^
    " | Last Log Index: "^(Index.to_string s.lastlogIndex)^
    " | Last Log Term: "^(Index.to_string s.lastlogTerm)^
    " | Commit Index: "^(Index.to_string s.commitIndex)^
    " | Client Sequence Number: "^(Int.to_string s.seqNum)^
    " | Outstanding Client Request: "^(string_of_option (fun (i,_) -> Index.to_string i ) s.outstanding_request)^"\n"^
    " | Replicated Log: "^(Log.to_string ~cmd_to_string:Mach.cmd_to_string s.log)^
    "\n-------------------------------------------------------------------------------------"
 (* sexp_of_t s |> Sexp.to_string *)

 (* let rec log_add original new_list = 
   (*combine logs, if two items with same index, give proirity to new *)
   match new_list with
   | (index,term,cmd)::rest -> log_add ((index,term,cmd)::(List.filter original ~f:(fun (i,_,_) -> not(index=i)))) rest
   | [] -> original *)


 let update_commitIndex match_list commitIndex log curr_term =
    let magority = (List.length match_list +1 / 2) in
    let discard (_,index) = 
      (if index > commitIndex then Some index else None) in
    let new_list = 
      List.sort ~cmp:(fun x y -> -1* (Index.compare x y))
        (List.filter_map ~f:discard match_list) in
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
    | VoteFrom id ->
        { s with votesGranted = id::s.votesGranted }
    | StepDown tm ->
        { s with mode=Follower;
        term = tm;
        votedFor = None;
        votesResponded=[];
        votesGranted=[];
        nextIndex  = [];
        matchIndex = []; 
        safety_monitor = (
            match s.mode with 
            | Candidate -> RaftMonitor.tick s.safety_monitor `StepDown_from_Candidate
            | Leader -> RaftMonitor.tick s.safety_monitor `StepDown_from_Leader 
            | Follower -> s.safety_monitor );
        }
    | StartCandidate -> 
        { s with mode=Candidate;
        votedFor = Some s.id;
        votesResponded=[];
        votesGranted=[s.id];
        term = (Index.succ s.term);
        safety_monitor =
            match s.mode with
            | Candidate -> RaftMonitor.tick s.safety_monitor `RestartElection
            | Follower -> RaftMonitor.tick s.safety_monitor `StartElection;
        }
    | SetTime t -> 
        { s with time=(MonoTime.store t)}
    | StartLeader -> 
        { s with mode=Leader;
        votedFor = None;
        (* hack for debug *)
       (* votesResponded=[]; *)
       (* votesGranted=[]; *)
        leader = Some s.id;
        nextIndex  = List.map s.allNodes ~f:(fun id -> (id,Index.succ (s.lastlogIndex)) );
        matchIndex = List.map s.allNodes ~f:(fun id -> (id,Index.init()) );
        safety_monitor = RaftMonitor.tick s.safety_monitor `WinElection;
        }
    | SetLeader ld ->
        { s with leader= Some ld}
    | SetTerm t ->
        assert (t >= s.term);
        { s with term = t;
          votedFor = None }
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
            { s with 
            matchIndex = new_matchIndex ;
            nextIndex = (List.Assoc.add s.nextIndex id (Index.succ index_new));
            commitIndex = update_commitIndex new_matchIndex s.commitIndex s.log s.term ; }
        | _ -> assert false )
    | AddClientRequest (index,res) ->
      {s with outstanding_request = Some (index,res); }
    | RemoveClientRes -> 
      { s with outstanding_request = None}


end
(*
module StateHandler =
  functor (MonoTime: Clock.TIME) ->
  functor (Mach: Statemach.MACHINE ) -> struct

module State = PureState(MonoTime)(Mach)

  type t = (IntID.t,State.t status ) List.Assoc.t

  let find sl id  = match (List.Assoc.find sl id) with
    | Some x -> x | None -> Notfound

  let find_wst sl id = match (find sl id) with
    | Live x | Down x -> x | Notfound -> exit 1

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
 
  let exists sl id = List.exists sl ~f:(fun (node_id,_) -> node_id=id) 

  let add sl id state = 
    match (find sl id) with
    | Notfound | Live _ -> (* adding new node, assume live or already live *) 
        List.Assoc.add sl id (Live state) 
    | Down _ -> (* updating a dead node *)
            exit 1
  
  let from_listassoc x = x

  let init n : t =
    let id_list = List.init n ~f:(IntID.from_int) in
    let remove x xs = List.filter xs ~f:(fun y -> not (x = y)) in 
    let gen_state id id_list = State.init id (remove id id_list) in
    List.map 
    ~f:(fun node_id -> node_id , (Live (gen_state node_id id_list))) id_list

  let check_condition sl ~f = 
    match (List.find sl ~f) with Some _ -> true | None -> false 

  let kill sl id time= 
    match (find sl id) with
    | Live s -> 
        let s_new = State.tick (SetTime time) s in
        List.Assoc.add sl id (Down s_new)
    | Down s -> exit 1 (* killing a down node *)

  let wake sl id time =
    match (find sl id) with 
    | Down s -> 
        let s_new = 
          State.tick Restart s 
          |> State.tick (SetTime time) in
        List.Assoc.add sl id (Live s_new)
    | Live s -> exit 1 (*waking a live node *)

  let check_safety _ = ()  
end
*)
module StateHandlerHist =
  functor (MonoTime: Clock.TIME) ->
  functor (Mach: Statemach.MACHINE ) -> struct

module State = PureState(MonoTime)(Mach)

  type t = (IntID.t,State.t status list) List.Assoc.t
  (*this is only created by init so we are assuming list always have at least
   * one element, hence use of hd_exn*)

  let append sl id (s: State.t status) = 
    List.Assoc.add sl id (s::(List.Assoc.find_exn sl id))

  let find sl id  = match (List.Assoc.find sl id) with
    | Some x -> List.hd_exn x
    | None -> Notfound

    (*like find but also strips status *)
  let find_wst sl id = match (find sl id) with
    | Live x | Down x -> x | Notfound -> exit 1

  (* returns a list of state.t for all live nodes, useful for iterating over *)
  let get_live (sl:t) = 
    let f (_,s) = (match (List.hd_exn s) with
    | Live _ -> true 
    | Down _ | Notfound -> false ) in
    let live_list = List.filter sl ~f in
    let g (_,s) = (match (List.hd_exn s) with Live st -> st) in
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
          (* TODO: this condition is too strong *)
      List.for_all live ~f:(fun s -> s.leader=hd.leader && s.term=hd.term)
    else false
 
  let exists sl id = List.exists sl ~f:(fun (node_id,_) -> node_id=id) 

  let add sl id state = 
    match (find sl id) with
    | Notfound -> assert false
    | Live _ -> (* adding new node, assume live or already live *) 
        append sl id (Live state)
    | Down _ -> (* updating a dead node *)
            exit 1
  
(*  let from_listassoc x = x *)

  let init n : t =
    let id_list = List.init n ~f:(IntID.from_int) in
    let remove x xs = List.filter xs ~f:(fun y -> not (x = y)) in 
    let gen_state id id_list = State.init id (remove id id_list) in
    List.map 
    ~f:(fun node_id -> node_id , [(Live (gen_state node_id id_list))] ) id_list

  let check_condition sl ~f = 
    let f_new (a,b) = f (a, List.hd_exn b) in
    match (List.find sl ~f:f_new) with Some _ -> true | None -> false 

  let kill sl id time= 
    match (find sl id) with
    | Live s -> 
        let s_new = State.tick (SetTime time) s in
        append sl id (Down s_new)
    | Down s -> assert false (* killing a down node *)

  let wake sl id time =
    match (find sl id) with 
    | Down s -> 
        let s_new = 
          State.tick Restart s 
          |> State.tick (SetTime time)
          |> State.tick Set in
        append sl id (Live s_new)
    | Live s -> assert false (*waking a live node *)

  let rec get_live_states sl = 
    let live_only l = List.filter_map l 
      ~f:(function Live x -> Some x | Down _ -> None) in
    match sl with
    | (id,s_lst)::rs -> (live_only s_lst) @ get_live_states rs
    | [] -> [] 

  let rec election_safety (states :State.t list) lds =
    match states with
    | s::rest -> ( match s.leader,(List.Assoc.find lds s.term) with
                 | (Some ldx, Some ldy) when ldx=ldy -> 
                     election_safety rest lds
                 | (None, _) -> election_safety rest lds    
                 | (Some ldx, None) -> election_safety rest 
                     (List.Assoc.add lds s.term ldx) 
                 | _ -> assert false )
    | [] -> true


  let check_safety sl =
    let states = get_live_states sl in
    let _ = election_safety states [] in
    ()

  let get_leader sl =
    match List.hd (get_live sl) with
    | None -> None
    | Some state ->
        match (state.leader) with
        | None -> None 
        | Some leader_id -> Some (find_wst sl leader_id)


end
