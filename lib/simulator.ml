open Core.Std
open Common
open Eventlst 

(* [RaftSim] is a main body of the implementation, it handles the simulation, the
 * core protcol implementation and communication. This aspects need to be
 * divided up but everything depend on the State module with a functor *)

module RaftSim = 
  functor (MonoTime: Clock.TIME) ->
  functor (Mach: Statemach.MACHINE) ->
  functor (P: PARAMETERS) -> struct    

(*Setting up the StateList and State, which hold the state for the nodes *)
module StateList = Env.StateHandlerHist(MonoTime)(Mach)
module State = StateList.State

module Client = Client.ClientHandler(MonoTime)(Mach)

open Event (*needed to quickly access the event constructors like RaftEvent and SimulationEvent *)

module EventItem = struct
  type t = (MonoTime.t,IntID.t,State.t,Client.t) Event.t
  let compare = Event.compare
end

module EventList = (LinkedList(EventItem) : EVENTLIST with type item = EventItem.t)

  type eventsig = State.t -> (State.t * EventList.item list)
  type clientsig = Client.t -> (Client.t * EventList.item list)


let debug x = if (P.debug_mode) then (printf " %s  \n" x) else ()

(* TODO: consider spliting this up into 3 functions*)
let timeout (m:role) = MonoTime.span_of_float (P.timeout () m)

type datacollection = {
  mutable pkts: int; 
  mutable client_pkts: int; 
  mutable firstele: MonoTime.t option;
  mutable full_latency: (MonoTime.t option ) * (MonoTime.span list);
    }

let data = {
  pkts=0; 
  client_pkts=0; 
  firstele=None; 
  full_latency= (None,[]);
  }


let client_latency opt = 
  match opt with
  | `Start start_time -> (
    match data.full_latency with
    | (None,rest) -> 
        debug ("Starting Client Latency Timer at "^MonoTime.to_string start_time);
        data.full_latency <- (Some start_time,rest)
    | (Some _ ,_) -> () )
  | `Stop stop_time -> (
    match data.full_latency with
    | (None,rest) -> assert false
    | (Some start_time ,rest) -> 
      let time_diff = MonoTime.diff stop_time start_time in
      debug ("Stopping Client Latency Timer at "^MonoTime.to_string stop_time);
      data.full_latency <- ( None, time_diff::rest ) )

module type COMMS = sig
    val unicast_replica: IntID.t -> MonoTime.t -> eventsig -> EventList.item
    val unicast_client: MonoTime.t -> clientsig -> EventList.item
    val broadcast: IntID.t list -> MonoTime.t -> eventsig -> EventList.item list
  end

module type RAFT = sig

  (** Helper Functions - Called by the EventFunction to perform some computation
    at that specific time and location *)

  val startCand: eventsig
  val dispatchAppendEntries: eventsig
  val startLeader: eventsig
  val refreshTimer: eventsig

  (** Event Functions , this have some kinda terporal/spacail info => 
     get stored in the EventList **)
  val checkTimer: int -> eventsig
  val startFollow: Index.t -> eventsig
  val requestVoteRq: Rpcs.RequestVoteArg.t -> eventsig
  val requestVoteRs: Rpcs.RequestVoteRes.t -> IntID.t -> eventsig
  val appendEntriesRq: Rpcs.AppendEntriesArg.t -> eventsig
  val appendEntriesRs: Rpcs.AppendEntriesRes.t -> IntID.t -> eventsig
  val clientRq: Rpcs.ClientArg.t -> eventsig

  (*Client events *)
  val clientRs: Rpcs.ClientRes.t -> clientsig
  val clientCommit: clientsig
  val checkTimer_client: int -> clientsig


end

module RaftImpl : RAFT = struct

module Comms : COMMS = struct 

let unicast_replica (dist: IntID.t) (t:MonoTime.t) (e) = 
  (*TODO: modify these to allow the user to specify some deley
   * distribution/bound *)
  let delay = MonoTime.span_of_float (P.pkt_delay()) in
  let arriv = MonoTime.add t delay in
  debug ("dispatching msg to "^(IntID.to_string dist) ^ " to arrive at "^
  (MonoTime.to_string arriv));
  data.pkts <- data.pkts + 1;
  RaftEvent (arriv ,dist ,e ) 

let unicast_client (t:MonoTime.t) (e) =
  let delay = MonoTime.span_of_float (P.pkt_delay()) in
  let arriv = MonoTime.add t delay in
  debug ("dispatching msg to client to arrive at "^
  (MonoTime.to_string arriv));
  data.client_pkts <- data.client_pkts + 1;
  ClientEvent (arriv, e)

let broadcast (dests:IntID.t list) (t:MonoTime.t) e  = 
  List.map dests ~f:(fun dst -> unicast_replica(dst) t e) 

end 


let checkElection (s:State.t) = 
  (* TODO: check exactly def of majority, maybe off by one error here *)
  (List.length s.votesGranted) > ((List.length s.allNodes)+1)/2


let rec  startCand (s:State.t) = 
  debug "Entering Candidate Mode / Restarting Electon";
  let s_new =  State.tick StartCandidate s in
  let args = Rpcs.RequestVoteArg.(
    { term = s_new.term;
      cand_id = s_new.id;
      last_index = s_new.lastlogIndex;
      last_term = s_new.lastlogTerm; 
    }) in
  let reqs = Comms.broadcast s_new.allNodes (s_new.time()) 
    (requestVoteRq args) in
  let s_new,timeout_event = refreshTimer s_new in
  (s_new, timeout_event@reqs )

and refreshTimer (s:State.t) = 
  let s_new = State.tick Set s in
  let timeout = MonoTime.add (s.time()) (timeout s.mode) in
  let (event:EventList.item) = RaftEvent (timeout, s.id, checkTimer s_new.timer) in 
  (s_new,[event])

and checkTimer (timer_num:int) (s:State.t) = 
  debug "Checking timer"; 
  if (s.timer=timer_num) then (
    debug "timer is valid, need execuating";
    match s.mode with
      | Follower -> 
          debug "follower hasn't heard from leader so becoming candidate"; 
          startCand s
      | Candidate -> 
          debug "election hasn't yet been won, as timer has benn invalidated"; 
          startCand s
      | Leader -> 
          debug "heartbeat dispatch due to timeout";
          dispatchAppendEntries s )
   else (
    debug "timer no longer valid"; 
    (s,[]) )

and checkTimer_client (timer_num:int) (c:Client.t) = 
  debug "Checking timer"; 
  if (c.timer=timer_num) then (
    debug "timer is valid, need to retry";
    let c_new = Client.tick NoResponse c in
    clientCommit c_new
  ) else (
    debug "timer no longer valid"; (c,[]) )

(* this function send heartbeat versions of AppendEntries to all other nodes *)
and dispatchAppendEntries (s:State.t) =
  let dispatch id = 
    let next_index = List.Assoc.find_exn s.nextIndex id in
    let (prev_index,prev_term) = Log.specific_index_term (Index.pred (List.Assoc.find_exn s.nextIndex id)) s.log in
    let args = Rpcs.AppendEntriesArg.(
      (*just a heartbeat message *)
        { term = s.term;
        lead_id = s.id;
        prevLogIndex = prev_index;
        prevLogTerm = prev_term;
        leaderCommit = s.commitIndex;
        entries = List.map (Log.get_entries next_index s.log) 
          ~f:(fun (i,t,c_sexp) -> (i,t,Mach.sexp_of_cmd c_sexp)) ; (*emprt list means this is heartbeat *)
        } ) in
  Comms.unicast_replica id (s.time()) 
    (appendEntriesRq args) in
  let reqs = List.map s.allNodes ~f:dispatch in
  let s_new,timeout_event = refreshTimer s in
  (s_new, timeout_event@reqs )


and startFollow term (s:State.t)  = debug "Entering Follower mode";
  (* used for setdown too so need to reset follower state *)
  let s = State.tick (StepDown term) s in 
  refreshTimer s

and startLeader (s:State.t) = debug "Election Won - Becoming Leader";
  let s_new,dispatch_pkts = dispatchAppendEntries (State.tick StartLeader s) in
  if (data.firstele=None) then data.firstele <- Some (s.time()) else ();
  (s_new, dispatch_pkts)

and stepDown incoming_mode term lead_id_maybe (s:State.t) = 
  let s_new,e_new = (
      if (term > s.term) then 
        match s.mode with  
      | Leader -> 
        startFollow term s
      | Candidate ->  startFollow term s
      | Follower -> 
        (State.tick (SetTerm term) s,[])  
      else if (term=s.term)&&(incoming_mode=Leader) then
        match s.mode with
      | Leader -> 
      debug "we have two leaders in one term";
      assert false
      | Candidate -> 
      debug "leader has been discovered, stopping election and step down";
      startFollow term s
      | Follower -> 
      debug "all is upto date"; 
      (s,[]) 
      else if (term=s.term)&&(incoming_mode=Candidate) then
         match s.mode with
      | Leader -> 
      debug "ignore candidate in this term";
      (s,[])
      | Candidate -> 
      debug "competing candidate";
      (s,[])
      | Follower -> 
      debug "all is well";
      (s,[]) 
       else (s,[])) in
    match lead_id_maybe with
    | Some id -> (State.tick (SetLeader id) s_new), e_new
    | None -> s_new,e_new
           
  (* TODO check if this case it handled correctly *)
  (* else if (term=s.term) && (s.mode=Leader) then startFollower term s *) 

  (* TODO ask anil why s needs to explicitly annotated to access its field *)

and requestVoteRq (args: Rpcs.RequestVoteArg.t) (s:State.t) =
  debug ("I've got a vote request from: "^ IntID.to_string args.cand_id^ 
         " term number: "^Index.to_string args.term);
  debug (Rpcs.RequestVoteArg.to_string args);
  let (s_new:State.t),e_new = stepDown Candidate args.term None s in
  let vote = 
    (args.term = s_new.term) &&  (
    (args.last_term > s_new.lastlogTerm ) || 
    ( (args.last_term = s_new.lastlogTerm ) && 
      (args.last_index >= s_new.lastlogIndex ) ))  && 
    (s_new.votedFor = None) &&
    (s.mode=Follower)
  in
  let s_new,e_new = 
   ( if vote then 
      refreshTimer (State.tick (Vote args.cand_id) s_new)
    else 
      (*no changes required *)
      s_new,e_new )in
  let res = 
    Rpcs.RequestVoteRes.(
      { term = s_new.term;
        votegranted = vote;
        replyto = args;
      }) in
  (s_new, (Comms.unicast_replica(args.cand_id) (s_new.time()) (requestVoteRs res
  s_new.id))::e_new)
  
and requestVoteRs (res: Rpcs.RequestVoteRes.t) id (s:State.t) = 
  debug ("Receive vote request reply from "^ IntID.to_string id );
  debug (Rpcs.RequestVoteRes.to_string res);
  (* TODO: consider how term check may effect old votes in the network *)
  if (res.term > s.term)  then startFollow res.term s
  else if (res.votegranted)&&(s.mode=Candidate)&&(s.term=res.term) 
    then begin 
      debug "Vote was granted";
      let s = State.tick (VoteFrom id) s in
      (if (checkElection s) then startLeader s else  (s, [])) end
    else (s, [])

and appendEntriesRq (args: Rpcs.AppendEntriesArg.t) (s:State.t) =
  (* in terms of log consistenty there our 4 states: 
     - perfect consisenty - just append entries
     - consistent to (term,index) - just remove extra entries and append
     - inconsistent at (term,index) - reply false
     - not even at (term,index) let - reply false *)
  debug ("Recieve hearbeat from "^IntID.to_string args.lead_id);
  debug (Rpcs.AppendEntriesArg.to_string args);
  if (args.term >= s.term) then 
    begin
    debug ("this AppendEntries is up to date and therefore valid");
    (*if required then stepDown from leader or follower or/and update term *)
    let (s_new:State.t),e_stepdown = 
        debug "handling the new term info";
        stepDown Leader args.term (Some args.lead_id) s in
    let s_new,e_timeout = refreshTimer s_new in
    debug("we are now in the same term");
    assert (s_new.mode=Follower);
    assert (s_new.term=args.term); 
    assert (s_new.leader=Some args.lead_id);
    (*TODO: investigate ordering of theres event, in particular commit Index and AppendEntries *)
    match (Log.consistency_check s_new.log args.prevLogIndex args.prevLogTerm) with
    | `Consistent-> 
          (* begin by removing surplus entries if required *)
          let s_new = (
            if (args.prevLogIndex=s_new.lastlogIndex)&&(args.prevLogTerm=s_new.lastlogTerm) then (
              debug("My log is perfectally consistent with the leader so no removals needed"); s_new
            ) else (   
              debug ("My log is consistent until prevLogIndex");
              (* RAFT SPEC: If an existing entry conflicts with a new one (same index 
              but different terms), delete the existing entry and all that 
              follow it (§5.3) *)
              State.tick (RemoveEntries (args.prevLogIndex,args.prevLogTerm)) s_new ) ) in 
          (* next add entries if required *)
          let s_new = (
            match args.entries with
            | [] -> debug("this is a heartbeat message"); s_new
            | x::xs -> debug ("this is not a heartbeat");
                let entries_cmd = List.map args.entries ~f:(fun (i,t,c_sexp) -> (i,t,Mach.cmd_of_sexp c_sexp)) in
                State.tick (AppendEntries entries_cmd) s_new  ) in
          (* RAFT SPEC: If leaderCommit > commitIndex, set commitIndex = min(leaderCommit, last log index) *)
          let s_new = (
              if (args.leaderCommit > s.commitIndex) then (
                debug "updating commitIndex";
                State.tick (Commit args.leaderCommit) s_new )
              else (
                debug "not updating commitIndex";
                s_new )) in
          (*works finished now sent reply *)
          let res = Rpcs.AppendEntriesRes.(
                { term = s_new.term; success=true; replyto = args; follower_id = s.id;} ) in
          (s_new,(Comms.unicast_replica(args.lead_id) (s_new.time()) (appendEntriesRs res s.id))::e_stepdown@e_timeout)
    | `Inconsistent -> (
          debug ("not consistent at prevLogIndex so fail");
        (* RAFT SPEC: Reply false if log doesn’t contain an entry at prevLogIndex
         whose term matches prevLogTerm (§5.3) *)
        (*TODO: workout when entry should actually be removed *)
       let res = Rpcs.AppendEntriesRes.(
        { term = s_new.term; success= false; replyto = args; follower_id = s.id;} ) in
        (s_new,(Comms.unicast_replica(args.lead_id) (s_new.time()) (appendEntriesRs res s.id))::e_stepdown@e_timeout) )

    end
  else
    (* RAFT SPEC: Reply false if term < currentTerm (§5.1) *)
    begin
    debug("this AppendEntries is behind the time, so ignore");
    let res = Rpcs.AppendEntriesRes.(
    { term = s.term; success= false; replyto = args; follower_id = s.id;} ) in
    (s,[Comms.unicast_replica(args.lead_id) (s.time()) (appendEntriesRs res s.id)])
    end

and appendEntriesRs (res: Rpcs.AppendEntriesRes.t) id (s:State.t) =
  debug (Rpcs.AppendEntriesRes.to_string res);
  (* 3 term cases: *)
  if (res.term>s.term) then (
    debug "step down, I'm no longer leader, ignore packet";
    (startFollow res.term s)
  ) else if (res.term<s.term) then (
    debug "this message is delayed so ignore it";
    (s,[])
  ) else (
    (* assert(s.mode=Leader); *)
    (* we have same terms so proceed *)
  match res.success with
  | true -> (
    debug "Successfully added to followers log"; 
    assert (s.mode=Leader);
    let new_index = (
      match res.replyto.entries with
     | [] -> (*it was a heartbeat so nextIndex is already correct *)
        res.replyto.prevLogIndex
     | (i,_,_)::_ -> i) in
    let s_new = State.tick (ReplicationSuccess (res.follower_id, new_index)) s in
    match (s_new.outstanding_request) with
    | Some (i,args) ->
        if (i>=s_new.commitIndex) then
          let s_new = State.tick RemoveClientRes s_new in
          let result = Mach.sexp_of_res (Mach.get_last_res s_new.state_mach) in
          let res = { Rpcs.ClientRes.success = Some result; node_id = s_new.id; leader = s_new.leader; replyto=args; } in
          (s_new, [Comms.unicast_client (s.time()) (clientRs res)])
        else (s_new,[])
    | None -> (s_new,[]) )
  | false -> 
    debug "Unsuccessful at adding to followers log";
    match s.mode with
    | Leader -> ( State.tick (ReplicationFailure (res.follower_id, res.replyto.prevLogIndex)) s, []) 
    | Candidate | Follower -> (s,[]) )


 and clientRq (args: Rpcs.ClientArg.t) (s:State.t) = 
  debug (Rpcs.ClientArg.to_string args);
  match s.mode with
  | Follower  | Candidate ->
    let res = { Rpcs.ClientRes.success = None; node_id = s.id; leader = s.leader; replyto = args; } in
    debug("I'm not the leader so can't commit");
    (s, [Comms.unicast_client (s.time()) (clientRs res)] )
 (* | Leader when args.seqNum = s.seqNum -> (
    debug "This command has already been committed";
    match (s_new.outstanding_request) with
    | Some (i,res) ->
          let s_new = State.tick RemoveClientRes s_new in
          (s_new, [Comms.unicast_client (s.time()) (clientRs res)])
    | None -> assert false) *)
  | Leader (* when args.seqNum > s.seqNum *) -> 
    let entry_index = Index.succ s.lastlogIndex in
    let log_entry = (entry_index, s.term, (Mach.cmd_of_sexp args.cmd)) in
    let to_string (i,t,c) = (Index.to_string i)^" "^(Index.to_string t)^" "^(Mach.cmd_to_string c) in
    let s_new = State.tick (AppendEntry log_entry) s in
    let s_new = State.tick (Commit entry_index) s_new in
    let s_new = State.tick (AddClientRequest (entry_index,args)) s_new in
    debug("I'm the leader so will try to commit command "^to_string log_entry);
    (s_new, [] )

and clientRs (res: Rpcs.ClientRes.t) (s:Client.t) = 
  (*TODO: make check if old packets mess this up *)
  debug ("Simulating clients response");
  debug (Rpcs.ClientRes.to_string res);
  let timer = MonoTime.add (s.time()) (MonoTime.span_of_int P.client_wait ) in
  match res.success with
  | Some result_sexp -> 
    let result = Mach.res_of_sexp result_sexp in 
    debug ("successfully committed result is "^Mach.res_to_string result); 
    debug ("Expected result is "^Mach.res_to_string (List.hd_exn s.expected_results));
    client_latency (`Stop (s.time()) );
    let s_new = Client.tick (Successful (res.node_id,result) )s in
    (s_new,[ClientEvent (timer,clientCommit) ]) 
  | None -> debug "unsucessful, try again";
    let s_new = Client.tick (Unsuccessful (res.node_id,res.leader) ) s in
    (s_new,[ClientEvent (timer,clientCommit)]) 


and clientCommit (s: Client.t) =
  match (s.workload) with
  | cmd::later -> 
    debug ("Client is attempting to commit "^(Mach.cmd_to_string cmd)) ;
    client_latency (`Start (s.time()) );
    let args = {Rpcs.ClientArg.cmd = (Mach.sexp_of_cmd cmd); } in
    let s_new = Client.tick Set s in
    let timeout = MonoTime.add (s_new.time()) (MonoTime.span_of_int 30) in
    let timer_check = ClientEvent (timeout,checkTimer_client s_new.timer) in
    (match s.leader with
    | Leader id -> 
      (s_new, [timer_check; Comms.unicast_replica id (s.time()) (clientRq args) ])
    | TryAsking (id::_) -> 
      (s_new, [timer_check; Comms.unicast_replica id (s.time()) (clientRq args) ]) )
  | [] -> 
    debug "successfully committed all commands "; (s,[])

end



let start_time = MonoTime.init()


let nxt_failure (t:MonoTime.t) = 
  match P.nxt_failure with
  | Some dl ->
    let delay = MonoTime.span_of_float (dl ()) in
    MonoTime.add t delay

let nxt_recover (t:MonoTime.t) = 
  match P.nxt_failure with
  | Some dl ->
  let delay = MonoTime.span_of_float (dl ()) in
  MonoTime.add t delay

let span_to_string (time:MonoTime.t) =
   let duration = MonoTime.diff (time) start_time in
    MonoTime.span_to_string (duration)

let state_span (sl:StateList.t) = 
  (* TODO: fix this so it finds the first live node *)
  (*let state = match (StateList.find sl (IntID.from_int 0)) with Live x -> x in*)
  match StateList.get_leader sl with
    | None -> assert false
    | Some (state) -> span_to_string (state.time()) 

let termination_output reason sl (cl: Client.t) =
  let time_str = match reason with
    | LeaderEst -> 
      state_span sl
    | WorkloadEmpty -> 
      span_to_string (cl.time()) 
    | Timeout -> 
      MonoTime.span_to_string (MonoTime.span_of_int P.term_time) in
  let first_election = 
    match data.firstele with 
      Some x -> MonoTime.to_string x 
      | None -> "" in
   let latency_list = List.rev (match data.full_latency with (_,lst) -> lst) in
 (* let term_str = Index.to_string StateList.get_leader term in *)
  "Reason: "^(termination_to_string reason)^
  "\n Time: "^time_str^
  "\n Replica Packets: "^(Int.to_string data.pkts)^
  "\n Client Packets: "^(Int.to_string data.client_pkts)^
  "\n Leader Established: "^first_election^
  "\n Client Latency: "^(List.to_string ~f:MonoTime.span_to_string latency_list)^
  "\n"

let wake (s:State.t) : EventList.item list =
  debug ((IntID.to_string s.id)^" node is restarting after failing");
  let timeout = MonoTime.add (s.time()) (timeout Follower) in
  let (event:EventList.item) = RaftEvent (timeout, s.id, RaftImpl.checkTimer s.timer) in 
  [(SimulationEvent (nxt_failure (s.time()), s.id, Kill)); event]

let kill (s:State.t) = 
  debug ((IntID.to_string s.id)^"node has failed");
  [SimulationEvent (nxt_recover (s.time()), s.id, Wake)]

let apply_RaftEvent (st: State.t status) (e: (MonoTime.t,IntID.t,State.t,Client.t) event) (t: MonoTime.t) 
   : (State.t * EventList.item list) option =
  (* wait used in realtime simulation, just instant unit for DES *)
  MonoTime.wait_until t;
  match st with 
  | Live s ->
     debug ("Start Simulating event on node "^(IntID.to_string s.id));
     let s = State.tick (SetTime t) s in
     let s_new,e_new = e s in
     debug (State.print s_new);
     Some (s_new,e_new)
 | Down s -> (*node is down so event is lost *)
     debug ("Discard event, for node "^(IntID.to_string s.id));
     None
 | Notfound -> assert false

let apply_SimulationEvent (sl: StateList.t) (e: failures) (t: MonoTime.t) (id:IntID.t) 
   : StateList.t * EventList.item list =
  MonoTime.wait_until t;
  (
  match e with 
  | Wake -> 
      debug ("Simulating recovery for node "^(IntID.to_string id)); 
      let sl_new = StateList.wake sl id t in (* handle state changes *)
      let s_new = StateList.find_wst sl_new id in 
      let e_new = wake s_new  in(*handle waking events *)
      debug (State.print s_new);
      (sl_new,e_new)
  | Kill ->
      debug ("Simulating failure for node "^(IntID.to_string id)); 
      let sl_new = StateList.kill sl id t in
      let e_new = kill (StateList.find_wst sl_new id) in
      (sl_new,e_new) )

let apply_ClientEvent (cl: Client.t) (e: (MonoTime.t,IntID.t,State.t,Client.t) client) (t: MonoTime.t) 
   : (Client.t * EventList.item list) =
  (* wait used in realtime simulation, just instant unit for DES *)
  MonoTime.wait_until t;
  debug "Simulating Client Event";
     let cl = Client.tick (SetTime t) cl in
     let cl_new,e_new = e cl in
     debug (Client.print cl_new);
     (cl_new,e_new)


let terminate reason sl cl = 
  (match reason with
  | LeaderEst -> debug "terminating as leader has been agreed"
  | WorkloadEmpty -> debug "terminating as all commands have been commited " 
  | Timeout -> debug "terminating as terminate time has been reached");
  StateList.check_safety sl;
  termination_output reason sl cl

(* Main excuation cycle *)  
let rec run_multi
  (sl: StateList.t) 
  (el: EventList.t)
  (cl: Client.t)  =
  (* checking termination condition for tests *)
    if (P.term_conditions LeaderEst)&&(StateList.leader_agreed sl) then 
    terminate LeaderEst sl cl
    else if (P.term_conditions WorkloadEmpty)&&(cl.workload=[]) then
    terminate WorkloadEmpty sl cl
    else 
  (* we will not be terminating as the term condition has been reached so get
   * the next event in the event queue and apply it *)
  match EventList.hd el with
  | None -> 
      debug "terminating as no events remaining"; 
      assert false (* debug "terminating as no events remain"; (span_to_string (cl.time())) *)
  (* next event is a simulated failure/recovery *)
  | Some (SimulationEvent (t,id,e),els) -> 
      let sl_new, el_new = apply_SimulationEvent sl e t id in
      assert (
        match StateList.find sl_new id, e with 
        | Live _, Wake | Down _, Kill -> true
        | _ -> false);
      run_multi sl_new (EventList.add el_new els) cl
  (* next event is some computation at a node *)
  | Some (RaftEvent (t,id,e),els) -> (
      match (apply_RaftEvent (StateList.find sl id) e t) with
      | Some (s_new,el_new) -> run_multi (StateList.add sl id s_new) (EventList.add el_new els) cl
      | None -> run_multi sl els cl )

 | Some (ClientEvent (t,e),els) -> (
      let (cl_new,el_new) = apply_ClientEvent cl e t in
      run_multi sl (EventList.add el_new els) cl_new )

  | Some (Terminate t,_) -> 
      terminate Timeout sl cl


let init_eventlist num  :EventList.t  =  
  let initial_RaftEvent = 
    List.init num ~f:(fun i ->
    RaftEvent (MonoTime.init(), IntID.from_int i, RaftImpl.startFollow (Index.init()) ) ) in
  let initial_SimulationEvent = 
    match P.nxt_failure with
    | Some _ ->
      List.init num ~f:(fun i -> SimulationEvent (nxt_failure (MonoTime.init()), IntID.from_int i, Kill)) 
    | None -> [] in
  let term_event = [Terminate (MonoTime.add start_time (MonoTime.span_of_int P.term_time))] in
  let inital_ClientEvent = [ClientEvent (MonoTime.init(), RaftImpl.clientCommit)] in
  EventList.init (initial_SimulationEvent@initial_RaftEvent@inital_ClientEvent@term_event)


let start () =
  debug "Raft Simulator is Starting Up ...";
  run_multi 
  (StateList.init P.nodes)  
  (init_eventlist P.nodes)
  (Client.init P.nodes P.workload_size)

end
