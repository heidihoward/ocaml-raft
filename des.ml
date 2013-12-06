open Core.Std
open Common
open Clock
open Env

module MonoTime = Clock.FakeTime
module State = Env.PureState(IntID)(MonoTime)(Index)(LogEntry)(ListLog)

module SimID : ID = struct
  (* type loc = (MonoTime.t,State.t) EventList.t option *)
  (* TODO ask anil about the get_loc type issue for tuples *)
  type msg = (MonoTime.t,State.t) Event.t
  type t = int *(msg list)
  let from_int x = (x,[])
  let to_int = fst
  let comp (t1,_) (t2,_) = (t1=t2)
  let to_string (x,_) = string_of_int x
(*  let get_loc = snd *)
(*  let set_loc (x,_) l = (x,l) *)
  let dispatch (x,msgs) msg = (x,msg::msgs)
  let collect = snd
end 

open EventList
open Event


module DEventSim = functor (Id:ID) -> struct

module MonoTime = Clock.FakeTime
module State = Env.PureState(Id)(MonoTime)(Index)(LogEntry)(ListLog)

let timeout = MonoTime.span_of_int 5

type checker = Timeout | Vote (*what event triggered a check of electron outcome *)

let rec incrTime s = (State.tick IncrementTime s, [])

and startCand (s:State.t) = debug "Entering Candidate Mode";
  let snew = State.tick (Vote s.id) s
          |> State.tick IncrementTerm
          |> State.tick StartCandidate in
  let reqs = List.map snew.allNodes 
    ~f:(fun rcv ->  E (snew.time, requestVoteRq snew.term snew.id snew.lastlogIndex
    snew.lastlogTerm rcv)) in
  let t = MonoTime.add snew.time timeout in
  (snew, E (t, checkElection Timeout)::reqs )

and checkTimer (s:State.t)  = debug "Checking heartbeat timer"; 
  if (s.mode = Follower) then
    (* if heartbeat is true, we have rec a packet in the last election timeout*)
    if s.heartbeat then 
      let t = MonoTime.add s.time timeout in
      (State.tick Reset s, [ E (t, checkTimer )]) 
    (* we have timedout so become candidate *)
    else (s,[E (s.time, startCand)])
  else (s,[])

and startFollow (s:State.t)  = debug "Entering Follower mode";
  let t = MonoTime.add s.time timeout in
  (s,[E (t, checkTimer)])

  (* TODO ask anil why s needs to explicitly annotated to access its field *)
and requestVoteRq term cand_id lst_index last_term rvc (s:State.t) =
  debug ("Dispatch request to "^ Id.to_string rvc );
  (* Simulated Responses *)
  (s,[E (MonoTime.succ s.time, requestVoteRs term true rvc )])
  
and requestVoteRs term voteGranted id (s:State.t) = 
  debug ("Receive request reply from "^ Id.to_string id );
  if (term > s.term) 
  then (State.tick (StepDown term) s,[ E (s.time,startFollow)]) 
  else if (voteGranted) 
  then (State.tick (VoteFrom id) s, [E (s.time,checkElection Timeout)])
  else (s, [])

and checkElection c s =
  debug "Check Timer";
  match c with 
  | Timeout -> (*TODO: check electon outcome *) (s,[])
  | Vote -> (* TODO:check election outcome *) (s,[])


let rec run ~term (s:State.t) (el: (MonoTime.t,State.t) EventList.t)  = 
  (* checking for termination conditions *)
  match el with 
  | []-> debug "terminating as no events remain" 
  | l -> ( 
    if ( match term with | Some tt -> (tt=s.time) | _ -> false )
      then debug "terminating as terminate time has been reached"
      else (
    match (EventList.find s.time l) with
    | Some (e,ls) -> 
        let s_new,e_new = e s in
        State.print s_new;
        run ~term s_new (EventList.add e_new ls)
    | None -> 
        debug "Incrementing Time"; State.print s;
        run ~term (State.tick IncrementTime s) el )) 

let run_multi l: (State.t * (MonoTime.t,State.t) EventList.t) list =
  let f ((s,el):(State.t * (MonoTime.t,State.t) EventList.t)) = 
    run  ~term:(Some (MonoTime.succ s.time)) s el in
  (* run each node for one time unit *)
  let l_new =  List.map ~f l in 
  (* collect events across the nodes *)
  let all_ids = List.map ~f:(fun (s,_) ->  s.allNodes) l_new 
             |> Caml.List.flatten in
  let msgs n = List.map ~f:(fun node -> if (Id.to_int node = n) then Id.collect
              else []) |> Caml.List.flatten in
  List.map ~f:(fun n (s,el) -> (s,EventList.add el (msgs n))) l_new
  |> run_multi 
 


end

module DES =  DEventSim(IntID)
module DESmulti = DEventSim(SimID)

let eventlist = DES.( [E (MonoTime.init(), startFollow);
                  E (MonoTime.t_of_int 30, incrTime)] )

let main = 
  DES.run 
  ~term:(Some (MonoTime.t_of_int 5)) 
  (DES.State.init()) 
  (EventList.from_list eventlist)

 (* DESmulti.run_multi *)
