open Core.Std
open Common


let () =
  let module Par = (struct
    let nodes = 2
    let timeout () = function
      | Follower -> NumberGen.string_to_dist "Uniform-150-300" ()
      | Candidate -> NumberGen.string_to_dist "Fixed-50" ()
      | Leader -> NumberGen.string_to_dist "Fixed-50" ()
    let pkt_delay = NumberGen.string_to_dist "Fixed-6"
    let debug_mode = true 
    let json_mode = false
    let nxt_failure = None
    let nxt_recover = None
    let term_conditions = function
      | LeaderEst -> true
      | WorkloadEmpty -> false
    let workload_size = 0
    let term_time = 1000
    let client_wait_success = 0
    let client_wait_failure = 0
    let client_timeout = 100
    let backoff = false
    let loss = 0.0
    let hist = false
    let cons = false
  end : PARAMETERS) in 
   
  let module DES =  
    Simulator.RaftSim(Clock.FakeTime)(Statemach.KeyValStr)(Par) in 
	DES.start()
	|> printf "%s"