open Core.Std
open Common
open MonoTime
module Mach = Statemach.KeyValStr

module ClientHandler =  struct

  (* for now we only have one client *)
  type cluster_leader = Leader of IntID.t | TryAsking of IntID.t list 

  let print_leader = function
  | Leader id -> ("Known: "^(IntID.to_string id))
  | TryAsking ids -> ("Unknown, trying: "^(List.to_string ~f:IntID.to_string ids))

  type t = { workload : Mach.cmd list;
             expected_results: Mach.res list;
             time : MonoTime.t;
             allNodes : IntID.t list;
             leader : cluster_leader;
             outstanding_request : Rpcs.ClientArg.t option;
             timer : int; 
             results : Mach.res list;
              }

  type statecall = 
    | Successful of IntID.t * Mach.res
    | Unsuccessful of (IntID.t * IntID.t option)
    | NoResponse
    | SetTime of MonoTime.t
    | Set

  let tick (sc:statecall) (state:t) =
  match sc with
  | Successful (leader_id,res) -> 
    assert(res=(List.hd_exn state.expected_results));
    { state with leader = Leader leader_id; 
      results = res::state.results;
      expected_results = List.tl_exn state.expected_results;
      workload = (List.tl_exn state.workload) }
  | Unsuccessful (id,leader_id) -> (
    match leader_id with 
    | Some new_leader_id -> 
      {state with leader= Leader new_leader_id}
    | None -> (
      match state.leader with
      | TryAsking nodes -> (
            match nodes with 
            | [] -> assert false
            | _::[] -> {state with leader = TryAsking state.allNodes} 
            | _::next -> {state with leader = TryAsking next}  )     
      | Leader old_lead_id -> 
          if (old_lead_id=id) then 
            {state with leader = TryAsking state.allNodes} 
          else
            state ))
  | NoResponse -> 
    {state with leader = TryAsking state.allNodes}
  | SetTime t -> 
    { state with time= t}
  | Set -> 
    { state with timer = Int.succ (state.timer)}

  let init nodes workload_size =
    let ids = List.init nodes ~f:IntID.from_int in
            { workload = Mach.gen_workload workload_size; 
              expected_results = Mach.gen_results workload_size; 
              time = MonoTime.init; 
              allNodes = ids ; 
              leader = TryAsking ids;
              timer = 0;
              outstanding_request = None;
              results = [];
              } 

  let print (s:t) = 
    "-- CLIENT STATE -----------------------------------------------\n"^
    " | Time: "^(MonoTime.to_string s.time)^
    " | All Nodes: "^(List.to_string ~f:IntID.to_string s.allNodes)^
    " | Leader: "^(print_leader s.leader)^"\n"^
    " | Workload: "^(List.to_string s.workload ~f:Mach.cmd_to_string )^
    " | Results: "^(List.to_string s.results ~f:Mach.res_to_string)^
    " | Expected Results: "^(List.to_string s.expected_results ~f:Mach.res_to_string)^
    "\n-------------------------------------------------------"

end