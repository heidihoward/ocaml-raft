open Core.Std
open Common

(*this functor takes more arguments than nessacary, but not sure about which
 * modules will have alternative inferences in the future, definitly ENTRY *)

module RaftSim = 
  functor (Id:NODE_ID) -> 
  functor (MonoTime: Clock.TIME) ->
  functor (Entry: ENTRY) ->
  functor (L:LOG) ->
  functor (P:PARAMETERS) -> struct

module StateList = Env.StateHandler(Id)(MonoTime)(Entry)(L)
module State = StateList.State
open Event (*needed to quickly access the event constructor E *)

(* debug_active :=  P.debug_mode *)
let debug x = printf " %s \n" x

let start_time = MonoTime.init()

(* let () = Random.self_init () *)
(* TODO: check it one timeout should be used for other electons and followers*)
let timeout (m:role) = MonoTime.span_of_int (P.timeout m)
let nxt_failure (t:MonoTime.t) = 
  let delay = MonoTime.span_of_int ((Random.int 50)+1) in
  MonoTime.add t delay
let nxt_reset (t:MonoTime.t) = 
  let delay = MonoTime.span_of_int ((Random.int 50)+1) in
  MonoTime.add t delay

module Comms = struct 

let unicast (dist:Id.t) (t:MonoTime.t) e = 
  (*TODO: modify these to allow the user to specify some deley
   * distribution/bound *)
  let delay = MonoTime.span_of_int (P.pkt_delay()) in
  let arriv = MonoTime.add t delay in
  debug ("dispatching msg to "^(Id.to_string dist) ^ " to arrive at
  "^(MonoTime.to_string arriv));
  E (arriv ,dist ,e ) 

let broadcast (dests:Id.t list) (t:MonoTime.t) e  = 
  List.map dests ~f:(fun dst -> unicast dst t e) 

end 


let checkElection (s:State.t) = 
  (* TODO: check exactly def of majority, maybe off by one error here *)
  (List.length s.votesGranted) > ((List.length s.allNodes)+1)/2

type checker = Follower_Timeout of Index.t 
             | Candidate_Timeout of Index.t
             | Leader_Timeout of Index.t

(* let rec incrTime s = (State.tick IncrementTime s, []) *)

let rec  startCand (s:State.t) = 
  debug "Entering Candidate Mode / Restarting Electon";
  let snew =  State.tick StartCandidate s in
  let reqs = Comms.broadcast snew.allNodes (snew.time()) 
    (requestVoteRq snew.term snew.id snew.lastlogIndex
    snew.lastlogTerm) in
  let t = MonoTime.add (snew.time()) (timeout Candidate) in
  (snew, E (t, s.id, checkTimer (Candidate_Timeout snew.term) )::reqs )

and checkTimer c (s:State.t)  = debug "Checking timer"; 
  let next_timer c_new (s:State.t) = 
    let t =  MonoTime.add (s.time()) (timeout Follower) in
    (State.tick Reset s, [ E (t, s.id, checkTimer c_new )]) in
  (* TODO: this about the case where the nodes has gone to candidate and back to
   * follower, how do we check for this case *)
  match c,s.mode with
  | Follower_Timeout term, Follower when term = s.term -> 
    (* if heartbeat is true, we have rec a packet in the last election timeout*)
    if s.timer then next_timer (Follower_Timeout s.term) s  
    (* we have timedout so become candidate *)
    else (startCand s)
  | Candidate_Timeout term, Candidate when term = s.term -> (debug "restating
  election"; startCand s)
  | Leader_Timeout term, Leader when term = s.term -> (dispatchHeartbeat s)
  | _ -> debug "Timer no longer valid"; (s,[])

and dispatchHeartbeat (s:State.t) =
  let reqs = Comms.broadcast s.allNodes (s.time()) 
    (heartbeatRq s.term s.id) in
  let t = MonoTime.add (s.time()) (timeout Leader) in
  (s, E (t, s.id, checkTimer (Leader_Timeout s.term) )::reqs )


and startFollow term (s:State.t)  = debug "Entering Follower mode";
  (* used for setdown too so need to reset follower state *)
  let t = MonoTime.add (s.time()) (timeout Follower) in
  let s = State.tick (StepDown term) s in 
  (s,[ E (t, s.id,checkTimer (Follower_Timeout s.term) )])

and startLeader (s:State.t) = debug "Election Won - Becoming Leader";
  dispatchHeartbeat (State.tick StartLeader s)

and stepDown term (s:State.t) = 
  if (term > s.term) 
  then match s.mode with | Leader | Candidate -> startFollow term s
                         | Follower -> ((State.tick (SetTerm term) s),[])
  
  else (s,[])
  (* TODO check if this case it handled correctly *)
  (* else if (term=s.term) && (s.mode=Leader) then startFollower term s *) 

  (* TODO ask anil why s needs to explicitly annotated to access its field *)

and requestVoteRq term cand_id lst_index last_term (s:State.t) =
  debug ("I've got a vote request from: "^ Id.to_string cand_id^ 
         " term number: "^Index.to_string term);
  (* TODO: this is a Simulated Response so allows granting vote
   * , need todo properly *)
  let (s_new:State.t),e_new =  stepDown term s in
  let vote = (term = s_new.term) && (lst_index >= s.lastlogIndex ) 
    && (last_term >= s.lastlogTerm ) && (s.votedFor = None) in
  let s_new = 
   ( if vote then 
      (State.tick (Vote cand_id) s 
      |> State.tick Set )
    else 
      s_new )in
  (s_new, (Comms.unicast cand_id (s_new.time()) (requestVoteRs s_new.term vote
  s_new.id))::e_new)
  
and requestVoteRs term voteGranted id (s:State.t) = 
  debug ("Receive vote request reply from "^ Id.to_string id );
  (* TODO: consider how term check may effect old votes in the network *)
  if (term > s.term)  then startFollow term s
  else if (voteGranted) 
    then begin 
      debug "Vote was granted";
      let s = State.tick (VoteFrom id) s in
      (if (checkElection s) then startLeader s else  (s, [])) end
    else (s, [])

and heartbeatRq term lead_id (s:State.t) =
  debug ("Recieve hearbeat from "^Id.to_string lead_id);
  let (s_new:State.t),e_new = stepDown term s in
  if (term = s_new.term) then 
    let (s_new:State.t) = State.tick Set s |> State.tick (SetLeader lead_id) in
    (s_new,(Comms.unicast lead_id (s_new.time()) (heartbeatRs s_new.term ))::e_new)
  else 
    (s_new,(Comms.unicast lead_id (s_new.time()) (heartbeatRs s_new.term))::e_new)

and heartbeatRs term (s:State.t) =
  let s_new,e_new = stepDown term s in
  (s_new,e_new)


let finished (sl: StateList.t) =
  let leader,term = match (StateList.find sl (Id.from_int 0)) with 
    Live s -> s.leader,s.term in
  (* TODO ask anil about suppressing pattern-match not exhaustive but leave case
   * undealt with, i.e it really should occur, if it does, then excuation should
   * stop *)
  let f (_,(state:State.t status)) = 
    match state with 
    | Live s -> not ((s.leader = leader) || (s.term = term)) 
    | Down _ | Notfound -> false
  in 
  (* TODO: modify finish to check the number of live nodes is the majority *)
  StateList.check_condition sl ~f

let printline =  "---------------------------------------------------\n"

let get_time_span (sl:StateList.t) = 
  (* TODO: fix this so it finds the first live node *)
  let state = match (StateList.find sl (Id.from_int 0)) with Live x -> x in
  let duration = MonoTime.diff (state.time()) start_time in
  MonoTime.span_to_string (duration)

let wake (s:State.t) =
  debug "node is restarting after failing";
  let s_new = State.tick Reset s in
  let timer = MonoTime.add (s.time()) (timeout Follower) in
  let e_new = [ E (timer, s.id,checkTimer (Follower_Timeout s_new.term) );
                N (nxt_failure (s_new.time()), s.id, Kill) ] in
  (s_new,e_new)

let kill (s:State.t) = 
  debug "node has failed";
  (s,[N (nxt_reset (s.time()), s.id, Wake)])

(* Main excuation cycle *)  
let rec run_multi ~term
  (sl: StateList.t) 
  (el:(MonoTime.t,Id.t,State.t) EventList.t)  =
  (* checking termination condition for tests *)
    if (finished sl) 
    then begin
      debug "terminating as leader has been agreed";
       (* for graph gen. *) printf " %s \n" (get_time_span sl) end
    else
  match EventList.hd el with
  | None -> debug "terminating as no events remain"
  | Some (N (t,id,e),els) -> ( 
      MonoTime.wait_until t;
      let state = StateList.find sl id in
      match e,state with 
      | Wake,Down s ->  
        let s = State.tick (SetTime t) s in
        let s_new, e_new = wake s in
          run_multi ~term 
          (StateList.add sl id (Live s_new)) 
          (EventList.add e_new els)
      | Kill,Live s ->
        let s = State.tick (SetTime t) s in
        let s_new, e_new = kill s in
          run_multi ~term 
          (StateList.add sl id (Down s_new)) 
          (EventList.add e_new els)
      | _ -> run_multi ~term sl els )
  | Some (E (t,id,e),els) -> if (t>=term) 
    then debug "terminating as terminate time has been reached"
    (* will not be terminating so simluate event *)
    else  
      MonoTime.wait_until t;
      match (StateList.find sl id) with 
      | Live s -> 
        let s = State.tick (SetTime t) s in
        let s_new,el_new = e s in
        debug (State.print s_new); debug printline;
        run_multi ~term (StateList.add sl id (Live s_new)) (EventList.add el_new els) 
        (* Currently none case is not used but will used later to simulate
         * failure *)
      | Down _ | Notfound -> debug "node is unavaliable";
        run_multi ~term sl els

let init_eventlist num  :(MonoTime.t,Id.t,State.t) Event.t list  =  
  let initial = List.init num ~f:(fun i ->
    E (MonoTime.init(), Id.from_int i, startFollow (Index.init()) ) ) in
  let failure_sim = List.init num ~f:(fun i -> 
    N (nxt_failure (MonoTime.init()), Id.from_int i, Kill)) in
  EventList.from_list (initial)


let start () = 
  let time_intval = MonoTime.span_of_int P.termination in
  let time_now = MonoTime.init() in
  run_multi ~term:(MonoTime.add time_now time_intval ) 
  (StateList.init P.nodes)  
  (init_eventlist P.nodes)
end
