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
      | LeaderEst -> json |> member "termination" |> member "leader" |> to_bool
      | WorkloadEmpty -> json |> member "termination" |> member "client" |> to_bool

    let workload_size = 
      json |> member "workload" |> to_int_option 
      |> function | None -> 0 | Some x -> x

    let term_time = json |> member "termination" |> member "time" |> to_int

    let client_wait_success = 
      json 
      |> member "client" |> member "timer" 
      |> to_int_option 
      |> function | None -> 0 | Some x -> x

    let client_wait_failure =
      json 
      |> member "client" |> member "timer" 
      |> to_int_option 
      |> function | None -> 0 | Some x -> x

    let client_timeout =
      json 
      |> member "client" |> member "timer" 
      |> to_int_option 
      |> function | None -> 0 | Some x -> x

    let loss = json |> member "network" |> member "packetLoss" |> to_float

    let backoff = json |> member "modifications" |> member "exponentialBackoff" |> to_bool
    let hist = json |> member "modifications" |> member "checker" |> to_bool
    let cons = json |> member "modifications" |> member "conservative" |> to_bool
  end : PARAMETERS) in 
   
  let module DES =  Simulator.RaftSim(Statemach.KeyValStr)(Par) in 
  printf "%s\n" (Summary.to_string (DES.start ()))

let () =  run (from_file "lib/config.json")