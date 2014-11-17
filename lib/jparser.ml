open Core.Std
open Common
open Yojson.Basic
open Util


let to_distribution str json =
  json 
  |> member "timers" 
  |> to_list 
  |> List.find ~f:(fun x -> (=) "follower" (x |> member "name"|> to_string))
  |> function | Some x -> x
  |> (fun para -> NumberGen.string_to_dist 
      ("Uniform-"^(Int.to_string (member "min" para |> to_int))^"-"^
          (Int.to_string (member "max" para |> to_int)))) 


let run json =
  let module Par = 
  (struct
    let nodes = json 
      |> member "nodes" 
      |> to_int

    let possible_leaders = json 
      |> member "eligible" 
      |> to_int_option 
      |> function | None -> nodes | Some x -> x

    let timeout () = function
      | Leader -> (to_distribution "leader" json) ()
      | Follower -> (to_distribution "follower" json) ()
      | Candidate -> (to_distribution "candidate" json) ()

    let pkt_delay () = 7.0

    let debug_mode = json 
      |> member "output" 
      |> to_list |> filter_string 
      |> List.exists ~f:((=) "debug")
    let json_mode = json 
      |> member "output" 
      |> to_list |> filter_string 
      |> List.exists ~f:((=) "json")

    let nxt_failure = None
    let nxt_recover = None
    
    let term_conditions = function
      | LeaderEst -> true
      | WorkloadEmpty -> false
    let workload_size = 0
    let term_time = 1000
    let client_wait_success = 100
    let client_wait_failure = 100
    let client_timeout = 100
    let backoff = false
    let loss = 0.0
    let hist = false
    let cons = false
  end : PARAMETERS) in 
   
  let module DES =  Simulator.RaftSim(Clock.FakeTime)(Statemach.KeyValStr)(Par) in 
  printf "%s\n" (Summary.to_string (DES.start ()))

let () =  run (from_file "lib/config.json")