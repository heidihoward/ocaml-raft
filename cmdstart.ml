open Core.Std
open Common

type time = Discrete | Real 


let distribution = 
  Command.Spec.Arg_type.create NumberGen.string_to_dist

let run ~time ~nodes ~term ~debug_enabled ~iter ~data ~follower ~candidate ~leader ~delay ~failure ~recover =
  let module Par = (struct
    let nodes = nodes
    let timeout = function
      | Leader -> leader ()
      | Follower -> follower ()
      | Candidate -> candidate ()
    let pkt_delay = delay
    let termination = term 
    let debug_mode = debug_enabled 
    let nxt_failure = failure
    let nxt_recover = recover
  end : PARAMETERS) in 
   
  match time with
  | Discrete ->
  begin
  let module DES =  
    Simulator.RaftSim(IntID)(Clock.FakeTime)(LogEntry)(ListLog)(Par) in
  for i=1 to iter do 
    ignore(i); DES.start() 
  done end 
  | Real -> 
  begin
  let module DES =  
    Simulator.RaftSim(IntID)(Clock.RealTime)(LogEntry)(ListLog)(Par) in
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
        ~doc:"filename File to output data to as .data"
     +> flag "-follower" (required distribution)
        ~doc:"distribution Follower Statistical Distribution"
     +> flag "-candidate" (required distribution)
        ~doc:"distribution Candidate Statistical Distribution"
     +> flag "-leader" (required distribution)
        ~doc:"distribution Leader Statistical Distribution"
     +> flag "-delay" (required distribution)
        ~doc:"distribution Packet Delay Statistical Distribution"
     +> flag "-failure" (optional distribution)
        ~doc:"distribution Node failure Statistical Distribution"
     +> flag "-recover" (optional distribution)
        ~doc:"distribution Node recovery Statistical Distribution"

 )

let realtime =
  Command.basic
    ~summary:"Realtime Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see www.cl.cam.ac.uk/~hh360 for more information ")
  Command.Spec.(
    empty
     ++ common ()
      )
    (fun nodes term debug_enabled iter data follower candidate leader delay failure recover () ->  
      run ~time:Real ~nodes ~term ~debug_enabled ~iter ~data ~follower ~candidate ~leader ~delay ~failure ~recover) 

let discrete =
  Command.basic
    ~summary:"Discrete Event Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see www.cl.cam.ac.uk/~hh360 for more information ")
  Command.Spec.(
    empty
     ++ common ()
      )
    (fun nodes term debug_enabled iter data follower candidate leader delay failure recover () ->  
      run ~time:Discrete ~nodes ~term ~debug_enabled ~iter ~data ~follower ~candidate ~leader ~delay ~failure ~recover) 

let () =  
  ["realtime",realtime;"discrete",discrete]
  |> Command.group ~summary:"Discrete Event Simulator & Realtime Simulator for
  Raft's Leader Election"
  |> Command.run
