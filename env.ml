open Core.Std
open Common

(** [State] holds the state of one of the nodes in the Raft simulation, the
 * state is accessable via type t with is a private record, its only modifiable
 * via buy passing statecalls to the tick function *)
module type STATE =
      sig
        type time
        type statemach
        type cmd
        type t = private {
          (** Generic state as specified by the protocol *) 
          term : Index.t;
          mode : role;
          votedFor : IntID.t option;
          log : cmd ListLog.t;
          lastlogIndex : Index.t;
          lastlogTerm : Index.t;
          lastApplied : Index.t;
          votesResponded : IntID.t list;
          votesGranted : IntID.t list;
          nextIndex : Index.t;
          lastAgreeIndex : Index.t;
          (** Simulation specfic state, need removing/altering for real
           * implementation *)
          time : unit -> time;
          timer : bool; 
          (** this flag is used to indicate if event of a timer
          has happened since last checked, a better method for this should be
          used *)
          id : IntID.t;
          allNodes : IntID.t list;
          leader : IntID.t option;
          state_mach : statemach;
        } with sexp
        
        (** [statecall] are created to modify state *)
        type statecall =
            IncrementTerm
          | Reset
          | Set
          | Vote of IntID.t
          | StepDown of Index.t
          | VoteFrom of IntID.t
          | StartCandidate
          | StartLeader
          | SetTime of time
          | SetLeader of IntID.t
          | SetTerm of Index.t
          | Restart
          | Commit of cmd
        
        (** [init] id (list of other ids) will create the state *)
        val init : IntID.t -> IntID.t list -> t
        (** [print] turns state into string for debugging *)
        val print : t -> string
        (** [tick] is the only way to modify state *)
        val tick : statecall -> t -> t
      end

module PureState  = 
  functor (MonoTime: Clock.TIME) ->
  functor (Mach: Statemach.MACHINE) -> struct

        type time = MonoTime.t
        type statemach = Mach.t
        type cmd = Mach.cmd

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


