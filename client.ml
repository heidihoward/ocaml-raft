open Core.Std
open Common

module ClientHandler = 
  functor (MonoTime: Clock.TIME) ->
  functor (Mach: Statemach.MACHINE) -> struct

  (* for now we only have one client *)
  type cluster_leader = Leader of IntID.t | TryAsking of IntID.t list 

  let print_leader = function
  | Leader id -> ("Known: "^(IntID.to_string id))
  | TryAsking ids -> ("Unknown, trying: "^(List.to_string ~f:IntID.to_string ids))

  type t = { workload : Mach.cmd list;
             time : unit -> MonoTime.t;
             allNodes : IntID.t list;
             leader : cluster_leader;
              }

  type statecall = 
    | Successful of IntID.t 
    | Unsuccessful of (IntID.t * IntID.t option)
    | SetTime of MonoTime.t

  let tick (sc:statecall) (state:t) =
  match sc with
  | Successful leader_id -> 
    { state with leader = Leader leader_id; 
      workload = (List.tl_exn state.workload) }
  | Unsuccessful (id,leader_id) -> 
    (match leader_id with 
    | Some id -> 
      {state with leader= Leader id}
    | None -> (
      match state.allNodes with
      | current::next -> {state with leader = TryAsking next}
      | [] -> {state with leader = TryAsking state.allNodes} ))
  | SetTime t -> 
    { state with time=(MonoTime.store t)}

  let init nodes =
    let ids = List.map (List.range 1 nodes) ~f:IntID.from_int in
            { workload = Mach.sample_workload; 
              time = MonoTime.init; 
              allNodes = ids ; 
              leader = TryAsking ids} 

  let print (s:t) = 
    "-------------------------------------------------------\n"^
    " | Time: "^(MonoTime.to_string (s.time()))^
    " | All Nodes: "^(List.to_string ~f:IntID.to_string s.allNodes)^
    " | Leader: "^(print_leader s.leader)^"\n"^
    " | Workload: "^(List.to_string s.workload ~f:(Mach.cmd_to_string))^
    "\n-------------------------------------------------------"



end