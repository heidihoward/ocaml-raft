open Core.Std
open Common

let run_disctime ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max
~debug_enabled ~iter
  ~data =
  let module Par = (struct
    let nodes = nodes
    let timeout = function
      | Leader -> Discrete (NumberGen.fixed 5 ())
      | Follower | Candidate -> Discrete (NumberGen.uniform_int time_min
      time_max ())
    let pkt_delay () = Discrete (NumberGen.uniform_int delay_min delay_max())
    let termination = term 
    let debug_mode = debug_enabled 
    let nxt_failure () = Discrete (NumberGen.uniform_int 1 50 ())
    let nxt_recover () = Discrete (NumberGen.uniform_int 1 4 () )
  end : PARAMETERS) in 
   
  let module DES =  
    Simulator.RaftSim(IntID)(Clock.FakeTime)(LogEntry)(ListLog)(Par) in
  for i=1 to iter do 
    ignore(i); DES.start()
  done 

let run_realtime ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max
~debug_enabled ~iter
  ~data =
  let module Par = (struct
    let nodes = nodes
    let timeout = function
      | Leader -> Continous (NumberGen.fixed 5.0 ())
      | Follower | Candidate -> Continous (NumberGen.uniform_float time_min
      time_max ())
    let pkt_delay () = Continous (NumberGen.uniform_float delay_min delay_max ())
    let termination = term 
    let debug_mode = debug_enabled 
    let nxt_failure () = Continous (NumberGen.uniform_float 1.0 50.0 () )
    let nxt_recover () = Continous (NumberGen.uniform_float 1.0 4.0 () )
  end : PARAMETERS) in 
   
  let module DES =  
    Simulator.RaftSim(IntID)(Clock.RealTime)(LogEntry)(ListLog)(Par) in
  for i=1 to iter do 
    ignore(i); DES.start()
  done 


let common () =
    Command.Spec.(
      empty
      +> flag "-nodes" (required int) 
        ~doc:"int Number of nodes to simulate"
      +> flag "-term" (optional_with_default 5000 int)
        ~doc:"int The maxiumun number of terms before termination"
      +> flag "-d" no_arg
        ~doc:"Enable debug (disabled by default)"
      +> flag "-iter" (optional_with_default 1 int) 
        ~doc:"int Number of Simulations"
      +> flag "-data" (optional string) 
        ~doc:"filename File to output data to as .data" )

let realtime =
  Command.basic
    ~summary:"Realtime Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see www.cl.cam.ac.uk/~hh360 for more information ")
  Command.Spec.(
    empty
     ++ common ()
     +> flag "-time-min" (optional_with_default 100.0 float)
        ~doc:"int The minimum timeout used"
     +> flag "-time-max" (optional_with_default 150.0 float)
        ~doc:"int The max timeout used"
     +> flag "-delay-min" (optional_with_default 6.0 float)
        ~doc:"int The min packet delay"
     +> flag "-delay-max" (optional_with_default 8.0 float)
        ~doc:"int The max delay of packets" 
      )
    (fun nodes term debug_enabled iter data time_min time_max delay_min
    delay_max () ->  
      run_realtime ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max
      ~debug_enabled ~iter ~data) 

let discrete =
  Command.basic
    ~summary:"Discrete Event Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see www.cl.cam.ac.uk/~hh360 for more information ")
  Command.Spec.(
    empty
     ++ common ()
     +> flag "-time-min" (optional_with_default 100 int)
        ~doc:"int The minimum timeout used"
     +> flag "-time-max" (optional_with_default 150 int)
        ~doc:"int The max timeout used"
     +> flag "-delay-min" (optional_with_default 6 int)
        ~doc:"int The min packet delay"
     +> flag "-delay-max" (optional_with_default 8 int)
        ~doc:"int The max delay of packets" 
      )
    (fun nodes term debug_enabled iter data time_min time_max delay_min
    delay_max () ->  
      run_disctime ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max
      ~debug_enabled ~iter ~data) 


let () =  
  ["realtime",realtime;"discrete",discrete]
  |> Command.group ~summary:"Discrete Event Simulator & Realtime Simulator for
  Raft's Leader Election"
  |> Command.run
