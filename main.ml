open Core.Std

let state = ref state.init()

module MonoTime = struct
  type t = int
  let init () = 0
  let diff t1 t2 = abs(t1-t2)
  let iter t = t+1
end

module ID = struct
  type t = int
  let compare t1 t1 = (t1=t2) 
end

module LogEntry = struct
  type t = A | B | C
end

module Log = struct
  let init () = []
  type t = LogEntry.t list
  let append t x = t::x
end

module State = struct
  type t = 
    { currentTerm : MonoTime.t;
      state: role;
      votedFor: ID.t option;
      log: Log.t;
      lastApplied: MonoTime.t;
      votesResponded: ID.t list;
      votesGranded: ID.t list;
      nextIndex: MonoTime.t;
      lastAgreeIndex: MonoTime.t }
   
  type statecall = 
    | Apply  (* now safe to apply LastAppled to the state machine*)
    | StepDown of MonoTime.t (* you are out of date, become follower in this new 
    | StepUp 
  type role = Follower | Candidate | Leader

  let init = 
  let print s :state -> unit =

  let tick s tk : t -> statecall -> t =
  match tk with
    | Apply -> 



end

let RequestVoteRq term cand_id lst_index last_term =
  if (term 

let RequestVoteRs

let AppendEntriesRq

let AppendEntriesRs



let main = 
  init()

  
