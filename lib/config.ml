open Core.Std
open Common
open Summary

let follower_timeouts =
  [
  (12.0,24.0); 
  (25.0,50.0); 
  (50.0,100.0);
  (100.0,200.0);
  (150.0,151.0); 
  (150.0,155.0);
  (150.0,175.0);
  (150.0,200.0);
  (150.0,300.0);
  ]

let scale x = 100.0 *. x
let scale_int x = 100 * x

let run (min,max) =
  let module Par = (struct
    let nodes = 5
    let timeout () = function
      | Follower -> 
          NumberGen.uniform (scale min) (scale max) 1.0 ()
      | Candidate -> 
          NumberGen.uniform (scale min) (scale max) 1.0 ()
      | Leader -> NumberGen.fixed (scale min/.2.0) ()
    let pkt_delay = NumberGen.normal_discardneg (scale 7.0) (scale 2.0)
    let debug_mode = false
    let json_mode = false
    let nxt_failure = None
    let nxt_recover = None
    let term_conditions = function
      | LeaderEst -> true
      | WorkloadEmpty -> false
    let workload_size = 0
    let term_time = (scale_int 1000)
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

let run_and_extract (min,max) =
  let filename = sprintf "data/%.0f-%.0fresults.log" min max in
  let output_stream = open_out filename in
  for i=1 to 30 do 
    let results = run (min,max) in 
      sprintf "%s\n" results.leader_est
      |> output_string output_stream
  done;
  close_out_noerr output_stream


let () =
  ignore (List.map follower_timeouts ~f:run_and_extract)

