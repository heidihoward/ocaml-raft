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
  val from_list: Event.(('a,'b) t) list -> ('a,'b) t
  val to_list: t -> Event.('a,'b) t list
end
  

module EventList = struct

  type ('a,'b) t = Event.('a,'b) t  list

  let from_list x = x
  let to_list x = x

  let rec find x (l:eventlist) =  
    let f (t,_) = (x=t) in
    match (List.partition_tf l ~f) with
  | E (_,e)::ls -> Some e,ls
  | [] -> None 

  let add a (l:eventlist) = a@l
end

open EventList

let timeout = MonoTime.span_of_int 5

type checker = Timeout | Vote (*what event triggered a check of electron outcome *)

let rec incrTime s : Event.event = (State.tick s IncrementTime, [])

and startCand s : Event.event  = debug "Entering Candidate Mode";
  let snew = State.tick (State.tick s IncrementTerm) (Vote s.id ) in
  let reqs = List.map snew.allNodes 
    ~f:(fun rcv -> (snew.time, requestVoteRq snew.term snew.id snew.lastlogIndex
    snew.lastlogTerm rcv)) in
  let t = MonoTime.add snew.time timeout in
  (snew, Next ((t, checkElection Timeout)::reqs))

and checkTimer (s:State.t) : Event.event  = debug "Checking heartbeat timer"; 
  if (s.mode = Follower) then
    (* if heartbeat is true, we have rec a packet in the last election timeout*)
    if s.heartbeat then 
      let t = MonoTime.add s.time timeout in
      (State.tick s Reset, Next [(t, checkTimer )]) 
    (* we have timedout so become candidate *)
    else (s,Next [(s.time, startCand)])
  else (s,Next [])

and startFollow (s:State.t) : Event.event = debug "Entering Follower mode";
  let t = MonoTime.add s.time timeout in
  (s,Next [(t, checkTimer)])

and requestVoteRq term cand_id lst_index last_term rvc s =
  debug ("Dispatch request to"^ IntID.print rvc );
  (s,Next [])
  
and requestVoteRs term voteGranted id (s:State.t) = 
  if (term > s.term) 
  then (State.tick s (StepDown term) ,Next [(s.time,startFollow)]) 
  else if (voteGranted) 
  then (State.tick s (VoteFrom id), Next [(s.time,checkElection Timeout)])
  else (s, Next [])

and checkElection c s = 
  match c with 
  | Timeout -> (*TODO: check electon outcome *) (s,Next [])
  | Vote -> (* TODO:check election outcome *) (s,Next [])

let eventlist = Next [(MonoTime.init(), startFollow);
                 (MonoTime.t_of_int 30, incrTime)]

  let rec run (s:State.t) (el:EventList.t) = match EventList.find State.t el with
  | Some (e,ls) -> let s_new,e_new = e s in
      run s_new (EventList.add e_new ls)
  | None -> run (State.tick s IncrementTime) el


  let main = run (State.init()) (EventList.from_list eventlist)
