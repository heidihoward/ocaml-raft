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

module type COMMS = sig
    val unicast_replica: IntID.t -> MonoTime.t -> eventsig -> EventList.item
    val unicast_client: MonoTime.t -> clientsig -> EventList.item
    val broadcast: IntID.t list -> MonoTime.t -> eventsig -> EventList.item list
  end

module type RAFT = sig


  type checker = Follower_Timeout of Index.t 
             | Candidate_Timeout of Index.t
             | Leader_Timeout of Index.t

  (** Helper Functions - Called by the EventFunction to perform some computation
    at that specific time and location *)

  val startCand: eventsig
  val dispatchAppendEntries: eventsig
  val startLeader: eventsig

  (** Event Functions , this have some kinda terporal/spacail info => 
     get stored in the EventList **)
  val checkTimer: checker -> eventsig
  val startFollow: Index.t -> eventsig
  val requestVoteRq: Rpcs.RequestVoteArg.t -> eventsig
  val requestVoteRs: Rpcs.RequestVoteRes.t -> IntID.t -> eventsig
  val appendEntriesRq: Rpcs.AppendEntriesArg.t -> eventsig
  val appendEntriesRs: Rpcs.AppendEntriesRes.t -> IntID.t -> eventsig
  val clientRq: Rpcs.ClientArg.t -> eventsig
  val clientRs: Rpcs.ClientRes.t -> clientsig
  val clientCommit: clientsig

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
  RaftEvent (arriv ,dist ,e ) 

let unicast_client (t:MonoTime.t) (e) =
  let delay = MonoTime.span_of_float (P.pkt_delay()) in
  let arriv = MonoTime.add t delay in
  debug ("dispatching msg to client to arrive at "^
  (MonoTime.to_string arriv));
  ClientEvent (arriv, e)

let broadcast (dests:IntID.t list) (t:MonoTime.t) e  = 
  List.map dests ~f:(fun dst -> unicast_replica(dst) t e) 

end 


let checkElection (s:State.t) = 
  (* TODO: check exactly def of majority, maybe off by one error here *)
  (List.length s.votesGranted) > ((List.length s.allNodes)+1)/2

type checker = Follower_Timeout of Index.t 
             | Candidate_Timeout of Index.t
             | Leader_Timeout of Index.t

let rec  startCand (s:State.t) = 
  debug "Entering Candidate Mode / Restarting Electon";
  let snew =  State.tick StartCandidate s in
  let args = Rpcs.RequestVoteArg.(
    { term = snew.term;
      cand_id = snew.id;
      last_index = snew.lastlogIndex;
      last_term = snew.lastlogTerm; 
    }) in
  let reqs = Comms.broadcast snew.allNodes (snew.time()) 
    (requestVoteRq args) in
  let t = MonoTime.add (snew.time()) (timeout Candidate) in
  (snew, RaftEvent (t, s.id, checkTimer (Candidate_Timeout snew.term) )::reqs )

and checkTimer c (s:State.t)  = debug "Checking timer"; 
  let next_timer c_new (s:State.t) = 
    let t =  MonoTime.add (s.time()) (timeout Follower) in
    (State.tick Reset s, [ RaftEvent (t, s.id, checkTimer c_new )]) in
  (* TODO: this about the case where the nodes has gone to candidate and back to
   * follower, how do we check for this case *)
  match c,s.mode with
  | Follower_Timeout term, Follower when term = s.term -> 
    (* if AppendEntries is true, we have rec a packet in the last election timeout*)
    if s.timer then next_timer (Follower_Timeout s.term) s  
    (* we have timedout so become candidate *)
    else (startCand s)
  | Candidate_Timeout term, Candidate when term = s.term -> (debug "restating
  election"; startCand s)
  | Leader_Timeout term, Leader when term = s.term -> (dispatchAppendEntries s)
  | _ -> debug "Timer no longer valid"; (s,[])

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
  let t = MonoTime.add (s.time()) (timeout Leader) in
  (s, RaftEvent (t, s.id, checkTimer (Leader_Timeout s.term) )::reqs )


and startFollow term (s:State.t)  = debug "Entering Follower mode";
  (* used for setdown too so need to reset follower state *)
  let t = MonoTime.add (s.time()) (timeout Follower) in
  let s = State.tick (StepDown term) s in 
  (s,[ RaftEvent (t, s.id,checkTimer (Follower_Timeout s.term) )])

and startLeader (s:State.t) = debug "Election Won - Becoming Leader";
  dispatchAppendEntries (State.tick StartLeader s)

and stepDown term lead_id_maybe (s:State.t) = 
  if (term > s.term) then 
    let s_new,e_new = (
      match s.mode with  
      | Leader | Candidate -> 
        startFollow term s
      | Follower -> 
        (State.tick (SetTerm term) s,[])  ) in
    match lead_id_maybe with
    | Some id -> (State.tick (SetLeader id) s_new), e_new
    | None -> s_new,e_new
  else (s,[])
           
  (* TODO check if this case it handled correctly *)
  (* else if (term=s.term) && (s.mode=Leader) then startFollower term s *) 

  (* TODO ask anil why s needs to explicitly annotated to access its field *)

and requestVoteRq (args: Rpcs.RequestVoteArg.t) (s:State.t) =
  debug ("I've got a vote request from: "^ IntID.to_string args.cand_id^ 
         " term number: "^Index.to_string args.term);
  debug (Rpcs.RequestVoteArg.to_string args);
  (* TODO: this is a Simulated Response so allows granting vote
   * , need todo properly *)
  let (s_new:State.t),e_new = stepDown args.term None s in
  let vote = (args.term = s_new.term) && (args.last_index >= s.lastlogIndex ) 
    && (args.last_term >= s.lastlogTerm ) && (s.votedFor = None) in
  let s_new = 
   ( if vote then 
      (State.tick (Vote args.cand_id) s 
      |> State.tick Set )
    else 
      s_new )in
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
  else if (res.votegranted) 
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
    let (s_new:State.t),e_new = 
        debug "handling the new term info";
        stepDown args.term (Some args.lead_id) s in
    debug("we are now in the same term");
    assert (s_new.mode=Follower && s_new.term=args.term && s_new.leader=Some args.lead_id);
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
              if (args.leaderCommit > s.commitIndex) then 
                State.tick (Commit args.leaderCommit) s_new 
              else
                s_new ) in
          (*works finished now sent reply *)
          let res = Rpcs.AppendEntriesRes.(
                { term = s_new.term; success=true; replyto = args; follower_id = s.id;} ) in
          (s_new,(Comms.unicast_replica(args.lead_id) (s_new.time()) (appendEntriesRs res s.id))::e_new)
    | `Inconsistent -> (
          debug ("not consistent at prevLogIndex so fail");
        (* RAFT SPEC: Reply false if log doesn’t contain an entry at prevLogIndex
         whose term matches prevLogTerm (§5.3) *)
        (*TODO: workout when entry should actually be removed *)
       let res = Rpcs.AppendEntriesRes.(
        { term = s_new.term; success= false; replyto = args; follower_id = s.id;} ) in
        (s_new,[Comms.unicast_replica(args.lead_id) (s_new.time()) (appendEntriesRs res s.id)]) )

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
    (stepDown res.term None s)
  ) else if (res.term<s.term) then (
    debug "this message is delayed so ignore it";
    (s,[])
  ) else (
    (* assert(s.mode=Leader); *)
    (* we have same terms so proceed *)
  match res.success with
  | true -> 
    debug "Successfully added to followers log"; 
    assert (s.mode=Leader);
    let new_index = (
      match res.replyto.entries with
     | [] -> (*it was a heartbeat so nextIndex is already correct *)
        res.replyto.prevLogIndex
     | (i,_,_)::_ -> i) in
    (State.tick (ReplicationSuccess (res.follower_id, new_index)) s,[])
  | false -> 
    debug "Unsuccessful at adding to followers log";
    match s.mode with
    | Leader -> ( State.tick (ReplicationFailure (res.follower_id, res.replyto.prevLogIndex)) s, []) 
    | Candidate | Follower -> (s,[]) )


 and clientRq (args: Rpcs.ClientArg.t) (s:State.t) = 
  debug (Rpcs.ClientArg.to_string args);
  match s.mode with
  | Follower  | Candidate ->
    let res = { Rpcs.ClientRes.success = false; node_id = s.id; leader = s.leader; replyto = args; } in
    debug("I'm not the leader so can't commit");
    (s, [Comms.unicast_client (s.time()) (clientRs res)] )
  | Leader -> 
    debug("I'm the leader so will pretend to commit");
    let entry_index = Index.succ s.lastlogIndex in
    let log_entry = (entry_index, s.term, (Mach.cmd_of_sexp args.cmd)) in
    let s_new = State.tick (AppendEntry log_entry) s in
    let s_new = State.tick (Commit entry_index) s_new in
    let res = { Rpcs.ClientRes.success = true; node_id = s.id; leader = s.leader; replyto=args; } in
    (s_new, [Comms.unicast_client (s.time()) (clientRs res)] )

and clientRs (res: Rpcs.ClientRes.t) (s:Client.t) = 
  debug ("Simulating clients response");
  debug (Rpcs.ClientRes.to_string res);
  let timer = MonoTime.add (s.time()) (MonoTime.span_of_int 5) in
  match res.success with
  | true -> debug "successfully committed"; 
    let s_new = Client.tick (Successful res.node_id) s in
    (s_new,[ClientEvent (timer,clientCommit) ]) 
  | false -> debug "unsucessful, try again";
    let s_new = Client.tick (Unsuccessful (res.node_id,res.leader) ) s in
    (s_new,[ClientEvent (timer,clientCommit)]) 


and clientCommit (s: Client.t) =
  match (s.workload) with
  | cmd::later -> 
    debug ("attempting to commit"^(Mach.cmd_to_string cmd)) ;
    let args = {Rpcs.ClientArg.cmd = (Mach.sexp_of_cmd cmd)} in
    (match s.leader with
    | Leader id -> (s, [Comms.unicast_replica id (s.time()) (clientRq args) ])
    | TryAsking (id::_) -> (s, [Comms.unicast_replica id (s.time()) (clientRq args) ]) )
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

let get_time_span (sl:StateList.t) = 
  (* TODO: fix this so it finds the first live node *)
  let state = match (StateList.find sl (IntID.from_int 0)) with Live x -> x in
  let duration = MonoTime.diff (state.time()) start_time in
  MonoTime.span_to_string (duration)

let wake (s:State.t) : EventList.item list =
  debug "node is restarting after failing";
  let timer = MonoTime.add (s.time()) (timeout Follower) in
   [ RaftEvent (timer, s.id,RaftImpl.checkTimer (Follower_Timeout s.term) );
     SimulationEvent (nxt_failure (s.time()), s.id, Kill) ]

let kill (s:State.t) = 
  debug "node has failed";
  [SimulationEvent (nxt_recover (s.time()), s.id, Wake)]

let apply_RaftEvent (st: State.t status) (e: (MonoTime.t,IntID.t,State.t,Client.t) event) (t: MonoTime.t) 
   : (State.t * EventList.item list) option =
  (* wait used in realtime simulation, just instant unit for DES *)
  MonoTime.wait_until t;
  match st with 
  | Live s ->
     let s = State.tick (SetTime t) s in
     let s_new,e_new = e s in
     debug (State.print s_new);
     Some (s_new,e_new)
 | Down s -> (*node is down so event is lost *)
     debug "node is unavaliable";
     None
 | Notfound -> assert false

let apply_SimulationEvent (sl: StateList.t) (e: failures) (t: MonoTime.t) (id:IntID.t) 
   : StateList.t * EventList.item list =
  MonoTime.wait_until t;
  match e with 
  | Wake -> 
      let sl_new = StateList.wake sl id t in (* handle state changes *)
      let e_new = wake (StateList.find_wst sl_new id)  in(*handle waking events *)
      (sl_new,e_new)
  | Kill ->
      let sl_new = StateList.kill sl id t in
      let e_new = wake (StateList.find_wst sl_new id) in
      (sl_new,e_new)

let apply_ClientEvent (cl: Client.t) (e: (MonoTime.t,IntID.t,State.t,Client.t) client) (t: MonoTime.t) 
   : (Client.t * EventList.item list) =
  (* wait used in realtime simulation, just instant unit for DES *)
  MonoTime.wait_until t;
     let cl = Client.tick (SetTime t) cl in
     let cl_new,e_new = e cl in
     debug (Client.print cl_new);
     (cl_new,e_new)


(* Main excuation cycle *)  
let rec run_multi ~term
  (sl: StateList.t) 
  (el: EventList.t)
  (cl: Client.t)  =
  (* checking termination condition for tests *)
(*    if (StateList.leader_agreed sl) 
    then begin
      debug "terminating as leader has been agreed";
       (* for graph gen.  printf " %s \n" (get_time_span sl); *)
        (get_time_span sl)
        end
    else *)
    if (cl.workload=[]) then
     begin
      debug "terminating as all commands have been commited ";
       (* for graph gen.  printf " %s \n" (get_time_span sl); *)
        (get_time_span sl)
        end
    else 
  (* we will not be terminating as the term condition has been reached so get
   * the next event in the event queue and apply it *)
  match EventList.hd el with
  | None -> debug "terminating as no events remain"; (get_time_span sl)
  (* next event is a simulated failure/recovery *)
  | Some (SimulationEvent (t,id,e),els) -> 
      let sl_new, el_new = apply_SimulationEvent sl e t id in
      StateList.check_safety sl;
      run_multi ~term sl_new (EventList.add el_new els) cl
  (* next event is some computation at a node *)
  | Some (RaftEvent (t,id,e),els) -> if (t>=term) 
    then begin debug "terminating as terminate time has been reached"; (get_time_span sl) end
    (* will not be terminating so simluate event *)
    else (
      match (apply_RaftEvent (StateList.find sl id) e t) with
      | Some (s_new,el_new) -> run_multi ~term (StateList.add sl id s_new) (EventList.add el_new els) cl
      | None -> run_multi ~term sl els cl )

 | Some (ClientEvent (t,e),els) -> if (t>=term) 
    then begin debug "terminating as terminate time has been reached"; (get_time_span sl) end
    (* will not be terminating so simluate event *)
    else  
      let (cl_new,el_new) = apply_ClientEvent cl e t in
      run_multi ~term sl (EventList.add el_new els) cl_new


let init_eventlist num  :EventList.t  =  
  let initial_RaftEvent = 
    List.init num ~f:(fun i ->
    RaftEvent (MonoTime.init(), IntID.from_int i, RaftImpl.startFollow (Index.init()) ) ) in
  let initial_SimulationEvent = 
    match P.nxt_failure with
    | Some _ ->
      List.init num ~f:(fun i -> SimulationEvent (nxt_failure (MonoTime.init()), IntID.from_int i, Kill)) 
    | None -> [] in
  let inital_ClientEvent = [ClientEvent (MonoTime.init(), RaftImpl.clientCommit)] in
  EventList.init (initial_SimulationEvent@initial_RaftEvent@inital_ClientEvent)


let start () =
  debug "Raft Simulator is Starting Up";
  let time_intval = MonoTime.span_of_int P.termination in
  let time_now = MonoTime.init() in
  run_multi ~term:(MonoTime.add time_now time_intval ) 
  (StateList.init P.nodes)  
  (init_eventlist P.nodes)
  (Client.init P.nodes)

end
