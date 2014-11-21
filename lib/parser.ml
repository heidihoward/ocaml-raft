open Core_kernel.Std
open Common
open Summary

type time = Discrete | Real 

let rec repeat n f = match n with
  | 0 -> ""
  | _ -> to_string (f()) ^" "^ (repeat (n-1) f)


let distribution = 
  Command.Spec.Arg_type.create NumberGen.string_to_dist

let run ~time ~nodes ~eligible ~term ~debug_enabled ~json_enabled ~iter ~data ~follower ~candidate ~leader ~delay 
~failure ~recover ~term_ele ~term_client ~cmds ~wait_succ ~wait_fail ~timeout_client ~backoff ~loss ~hist ~cons =
  let module Par = (struct
    let nodes = nodes
    let possible_leaders = match eligible with None -> nodes | Some x -> x
    let timeout () = function
      | Leader -> leader ()
      | Follower -> follower ()
      | Candidate -> candidate ()
    let pkt_delay = delay
    let debug_mode = debug_enabled 
    let json_mode = json_enabled
    let nxt_failure = failure
    let nxt_recover = recover
    let term_conditions = function
      | LeaderEst -> term_ele
      | WorkloadEmpty -> term_client
    let workload_size = cmds
    let term_time = term
    let client_wait_success = wait_succ
    let client_wait_failure = wait_fail
    let client_timeout = timeout_client
    let backoff = backoff
    let loss = loss
    let hist = hist
    let cons = cons
  end : PARAMETERS) in 
   
  match time with
  | Discrete ->
  begin
  let module DES =  
    Simulator.RaftSim(Par) in repeat iter DES.start
   end 
  | Real -> 
  begin
  let module DES =  
    Simulator.RaftSim(Par) in repeat iter DES.start end