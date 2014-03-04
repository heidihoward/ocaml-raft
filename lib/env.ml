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
          log : (Index.t * Index.t * Mach.cmd) list;
          lastlogIndex : Index.t;
          lastlogTerm : Index.t;
          commitIndex : Index.t;
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
   | Commit of Index.t
   | AppendEntries of Index.t * Index.t * (Index.t * Index.t * Mach.cmd) list
   | AppendEntry of (Index.t * Index.t * Mach.cmd)


  let init me all =
    { term = Index.init();
      mode = Follower;
      time = MonoTime.init;
      timer = false;
      votedFor = None;
      log = []; 
      lastlogIndex = Index.init();
      lastlogTerm = Index.init();
      commitIndex = Index.init();
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
      commitIndex = Index.init();
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
    " | Replicated Log: "^(List.to_string s.log 
      ~f:(fun (x,y,z) -> "Index: "^(Index.to_string x)^
                         "Term: "^(Index.to_string y)^
                         "Cmd: "^(Mach.cmd_to_string z)) )^
    "\n-------------------------------------------------------"
 (* sexp_of_t s |> Sexp.to_string *)

  let rec log_add original new_list = 
   (*combine logs, if two items with same index, give proirity to new *)
   match new_list with
   | (index,term,cmd)::rest -> log_add ((index,term,cmd)::(List.filter original ~f:(fun (i,_,_) -> not(index=i)))) rest
   | [] -> original

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
         assert(s.votedFor = None);
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
    (* RAFT SPEC: If leaderCommit > commitIndex, set commitIndex = min(leaderCommit, last log index) *)
    | Commit new_index -> (
        let new_index = 
          (if (new_index < s.lastlogIndex) then new_index else s.lastlogIndex) in
        let new_mach = 
        (List.filter s.log ~f:( fun (x,_,cmd) -> (x > s.commitIndex) && (x <= new_index ) ) 
        |> List.sort ~cmp:(fun (x,_,_) (y,_,_) -> Index.compare x y) 
        |> List.map ~f:(fun (_,_,x) -> x)
        |> Mach.commit_many s.state_mach) in
        { s with state_mach = new_mach; commitIndex=new_index} )
    | AppendEntries (last_index, last_term, entries) -> (
        match entries with
        | x::xs -> { s with log = log_add s.log entries; lastlogTerm=last_term; lastlogIndex=last_index }
        | [] -> s )
    | AppendEntry (index, term, cmd) -> 
       {s with log = (index,term,cmd)::s.log ; lastlogTerm=term; lastlogIndex=index}



end

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
    | Down s -> exit 1 (* killing a down node *)

  let wake sl id time =
    match (find sl id) with 
    | Down s -> 
        let s_new = 
          State.tick Restart s 
          |> State.tick (SetTime time) in
        append sl id (Live s_new)
    | Live s -> exit 1 (*waking a live node *)

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



end