open Core.Std
open Common
open Yojson.Basic
open Util


let to_option f x = 
  try Some (f x) with type_error -> None


let to_distribution json = 
  let distr = 
    member "distribution" json 
    |> to_string_option 
    |> function | None -> "Uniform" | Some d -> d in
    match distr with
    | "Fixed" -> 
      [distr; Int.to_string (member "value" json |> to_int)]
      |> String.concat ~sep:"-"
      |> NumberGen.string_to_dist 
    | _ ->
      [distr;
       Int.to_string (member "min" json |> to_int);
       Int.to_string (member "max" json |> to_int)]
      |> String.concat ~sep:"-" 
      |> NumberGen.string_to_dist
      
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
      | Leader -> 
        json |> member "timers" |> member "leader" 
        |> to_distribution |> fun f -> f()
      | Follower -> 
        json |> member "timers" |> member "follower" 
        |> to_distribution |> fun f -> f()
      | Candidate -> 
        json |> member "timers" |> member "candidate" 
        |> to_distribution |> fun f -> f()

    let pkt_delay = 
      json |> member "network" |> member "packetDelay" 
      |> to_distribution

    let debug_mode = json 
      |> member "output" 
      |> to_list |> filter_string 
      |> List.exists ~f:((=) "debug")
    let json_mode = json 
      |> member "output" 
      |> to_list |> filter_string 
      |> List.exists ~f:((=) "json")

    let nxt_failure = 
      json |> member "network" |> member "failure" 
      |> to_option to_distribution 

    let nxt_recover =
      json |> member "network" |> member "recover" 
      |> to_option to_distribution 

    let term_conditions = function
      | LeaderEst -> true
      | WorkloadEmpty -> false
    let workload_size = 0
    let term_time = 1000
    let client_wait_success = 100
    let client_wait_failure = 100
    let client_timeout = 100
    let backoff = false
    let loss = json |> member "network" |> member "packetLoss" |> to_float
    let hist = false
    let cons = false
  end : PARAMETERS) in 
   
  let module DES =  Simulator.RaftSim(Clock.FakeTime)(Statemach.KeyValStr)(Par) in 
  printf "%s\n" (Summary.to_string (DES.start ()))

let () =  run (from_file "lib/config.json")