open Core.Std
open Common

let run_disctime ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max
~debug_enabled ~iter
  ~data =
  let module Par = (struct
    let nodes = nodes
    let timeout = function
      | Leader -> NumberGen.fixed 5 ()
      | Follower | Candidate -> NumberGen.uniform_int time_min time_max ()
    let pkt_delay = NumberGen.uniform_int delay_min delay_max
    let termination = term 
    let debug_mode = if (debug_enabled) then debug else ignore 
    let write_data _ = ignore(data);()
    (* TODO implement the write data for leader election tests
      match data with 
      | Some file -> () 
      | None -> () *)
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
      | Leader -> NumberGen.fixed 5 ()
      | Follower | Candidate -> NumberGen.uniform_int time_min time_max ()
    let pkt_delay = NumberGen.uniform_int delay_min delay_max
    let termination = term 
    let debug_mode = if (debug_enabled) then debug else ignore 
    let write_data _ = ignore(data);()
    (* TODO implement the write data for leader election tests
      match data with 
      | Some file -> () 
      | None -> () *)
  end : PARAMETERS) in 
   
  let module DES =  
    Simulator.RaftSim(IntID)(Clock.RealTime)(LogEntry)(ListLog)(Par) in
  for i=1 to iter do 
    ignore(i); DES.start()
  done 


let common =
    Command.Spec.(
      empty
      +> flag "-nodes" (required int) 
        ~doc:"int Number of nodes to simulate"
      +> flag "-term" (optional_with_default 5000 int)
        ~doc:"int The maxiumun time before termination"
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
     ++ common
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
      run_realtime ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max
      ~debug_enabled ~iter ~data) 

let discrete =
  Command.basic
    ~summary:"Discrete Event Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see www.cl.cam.ac.uk/~hh360 for more information ")
  Command.Spec.(
    empty
     ++ common
     +> flag "-time-min" (optional_with_default 100 float)
        ~doc:"int The minimum timeout used"
     +> flag "-time-max" (optional_with_default 150 float)
        ~doc:"int The max timeout used"
     +> flag "-delay-min" (optional_with_default 6 float)
        ~doc:"int The min packet delay"
     +> flag "-delay-max" (optional_with_default 8 float)
        ~doc:"int The max delay of packets" 
      )
    (fun nodes term debug_enabled iter data time_min time_max delay_min
    delay_max () ->  
      run_disctime ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max
      ~debug_enabled ~iter ~data) 


let () =  
  ["realtime",realtime;"discrete",realtime]
  |> Command.group ~summary:"Discrete Event Simulator & Realtime Simulator for
  Raft's Leader Election"
  |> Command.run
