open Core.Std
open Common
open Clock
open Env

module MonoTime = Clock.FakeTime
module State = Env.PureState(IntID)(MonoTime)(Index)(LogEntry)(ListLog)

  module Event = struct 
  type ('a,'b) t = E of ('a * ('a,'b) event)
  and ('a,'b) event = ('b -> ('b * ('a,'b) t list))
  end 

module type EVENTLIST = sig
  type ('a,'b) t
  val from_list: ('a,'b) Event.t list -> ('a,'b) t
  val to_list: ('a,'b) t -> ('a,'b) Event.t list
  val find: 'a -> ('a,'b) t -> ('b * ('a,'b) t ) option
  val add: ('a,'b) Event.t -> ('a,'b) t -> ('a,'b) t
end
  

module EventList = struct

  type ('a,'b) t = ('a,'b) Event.t  list

  let from_list x = x
  let to_list x = x

  let find x l =  
    let open Event in
    let f = function | E (t,e) -> (x=t) in
    (* TODO write better implementation of find which returns list *)
    match (List.partition_tf l ~f) with
  | (E (_,e)::_,ls) -> Some (e,ls)
  | ([],_) -> None 

  let add a l = a@l
end

open EventList
open Event

let timeout = MonoTime.span_of_int 5

type checker = Timeout | Vote (*what event triggered a check of electron outcome *)

let rec incrTime s = (State.tick s IncrementTime, [])

and startCand s = debug "Entering Candidate Mode";
  let snew = State.tick (State.tick s IncrementTerm) (Vote s.id ) in
  let reqs = List.map snew.allNodes 
    ~f:(fun rcv -> E (snew.time, requestVoteRq snew.term snew.id snew.lastlogIndex
    snew.lastlogTerm rcv)) in
  let t = MonoTime.add snew.time timeout in
  (snew, E (t, checkElection Timeout)::reqs )

and checkTimer (s:State.t)  = debug "Checking heartbeat timer"; 
  if (s.mode = Follower) then
    (* if heartbeat is true, we have rec a packet in the last election timeout*)
    if s.heartbeat then 
      let t = MonoTime.add s.time timeout in
      (State.tick s Reset, [ E (t, checkTimer )]) 
    (* we have timedout so become candidate *)
    else (s,[E (s.time, startCand)])
  else (s,[])

and startFollow (s:State.t)  = debug "Entering Follower mode";
  let t = MonoTime.add s.time timeout in
  (s,[E (t, checkTimer)])

and requestVoteRq term cand_id lst_index last_term rvc s =
  debug ("Dispatch request to"^ IntID.to_string rvc );
  (s,[])
  
and requestVoteRs term voteGranted id (s:State.t) = 
  if (term > s.term) 
  then (State.tick s (StepDown term) ,[ E (s.time,startFollow)]) 
  else if (voteGranted) 
  then (State.tick s (VoteFrom id), [E (s.time,checkElection Timeout)])
  else (s, [])

and checkElection c s = 
  match c with 
  | Timeout -> (*TODO: check electon outcome *) (s,[])
  | Vote -> (* TODO:check election outcome *) (s,[])

let eventlist =  [E (MonoTime.init(), startFollow);
                  E (MonoTime.t_of_int 30, incrTime)]

let rec run (s:State.t) (el: (MonoTime.t,State.t) EventList.t)  = 
  match el with | [] -> debug "terminating" | l ->
    match (EventList.find s.time l) with
    | Some (e,ls) -> 
        let s_new,e_new = e s in
        State.print s_new;
        run s_new (EventList.add e_new ls)
    | None -> 
        debug "Incrementing Time"; State.print s;
        run (State.tick s IncrementTime) el

let main = run (State.init()) (EventList.from_list eventlist)
