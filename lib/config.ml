open Core.Std
open Common
open Summary

type figure = 
  | Original 
  | Fixed 
  | Expo
  | Combo

let fig2str = function
  | Original -> "org"
  | Fixed -> "fixed"
  | Expo -> "expo"
  | Combo -> "combo"

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

let correction min = 
  NumberGen.uniform (scale 0.0) (scale min/.2.0) 1.0 ()
  |> Float.to_int

let run (min,max) fig =
  let module Par = (struct
    let nodes = 5
    let possible_leaders = 5
    let timeout () = function
      | Follower -> 
          NumberGen.uniform (scale min) (scale max) 1.0 ()
      | Candidate -> (
          match fig with
          | Original | Expo -> 
             NumberGen.uniform (scale min) (scale max) 1.0 ()
          | Fixed | Combo -> 
            NumberGen.uniform (scale 23.0) (scale 46.0) 1.0 () )
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
    let term_time = (scale_int 100000)
    let client_wait_success = 0
    let client_wait_failure = 0
    let client_timeout = 100
    let backoff = 
      match fig with
      | Original | Fixed -> false
      | Expo | Combo -> true
    let loss = 0.0
    let hist = false
    let cons = false
  end : PARAMETERS) in 
   
  let module DES =  
    Simulator.RaftSim(Clock.FakeTime)(Statemach.KeyValStr)(Par) in 
  DES.start()

let run_and_extract fig (min,max) =
  let filename = sprintf "../ocaml-raft-data/raw/%s/%.0f-%.0fresults.log" (fig2str fig) min max in
  let output_stream = open_out filename in
  for i=1 to 100 do 
    let results = run (min,max) fig in
      match results.leader_est with
        | Some time -> (
            let correction_val = correction min in
            sprintf "%i\n" (time - correction_val)
            |> output_string output_stream )
        | None -> () 
  done;
  close_out_noerr output_stream


let () =
  let run_one fig = List.iter follower_timeouts ~f:(run_and_extract fig) in
  List.iter ~f:run_one [Original; Fixed; Expo; Combo]

