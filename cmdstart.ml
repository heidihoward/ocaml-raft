open Core.Std
open Common

type time = Discrete | Real

type dis = Fixed of value | Uniform of value * value | Exp of value

let string_to_dis time = 
  Command.Spec.Arg_type.create 
    (fun _ -> 
      match time with 
      | Discrete -> Fixed (Discrete 5.0)
      | Real -> Fixed (Continous 5.0) )

let run ~time ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max
~debug_enabled ~iter
  ~data =
  let module Par = (struct
    let nodes = nodes
    let timeout = function
      | Leader -> NumberGen.fixed (Continous 5.0) ()
      | Follower | Candidate -> 
          NumberGen.uniform time_min time_max ()
    let pkt_delay () = NumberGen.uniform delay_min delay_max ()
    let termination = term 
    let debug_mode = debug_enabled 
    let nxt_failure () = NumberGen.uniform (Continous 1.0) (Continous 50.0) () 
    let nxt_recover () = NumberGen.uniform (Continous 1.0) (Continous 4.0) () 
  end : PARAMETERS) in 
   
  match time with
  | Discrete ->
  begin
  let module DES =  
    Simulator.RaftSim(IntID)(Clock.RealTime)(LogEntry)(ListLog)(Par) in
  for i=1 to iter do 
    ignore(i); DES.start() 
  done end 
  | Real -> 
  begin
  let module DES =  
    Simulator.RaftSim(IntID)(Clock.FakeTime)(LogEntry)(ListLog)(Par) in
  for i=1 to iter do 
    ignore(i); DES.start() 
  done end
   
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
      run ~time:Real ~nodes ~term 
      ~time_min:(Continous time_min) 
      ~time_max:(Continous time_max)
      ~delay_min:(Continous delay_min) 
      ~delay_max:(Continous delay_max)
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
      run ~time:Discrete ~nodes ~term 
      ~time_min:(Discrete time_min) 
      ~time_max:(Discrete time_max) 
      ~delay_min:(Discrete delay_min)
      ~delay_max:(Discrete delay_max)
      ~debug_enabled ~iter ~data) 


let () =  
  ["realtime",realtime;"discrete",discrete]
  |> Command.group ~summary:"Discrete Event Simulator & Realtime Simulator for
  Raft's Leader Election"
  |> Command.run
