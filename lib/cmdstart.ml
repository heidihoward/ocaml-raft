open Core.Std
open Common


let common () =
    Command.Spec.(
      empty
      +> flag "-nodes" (required int) 
        ~doc:"int Number of nodes to simulate, must be 2 or greater"
      +> flag "-termOnTimeout" (optional_with_default 5000 int)
        ~doc:"int The maxiumun number of terms before termination"
      +> flag "-d" no_arg
        ~doc:"Enable debugging output (disabled by default)"
      +> flag "-iter" (optional_with_default 1 int) 
        ~doc:"int Number of Simulations to run (not working) "
      +> flag "-data" (optional string) 
        ~doc:"filename File to output data to as .data (currently not working)"
     +> flag "-follower" (required Parser.distribution)
        ~doc:"distribution Follower timeout Statistical Distribution, this gives
        the distribution of the follower timeout which lead to the start of an
        electon"
     +> flag "-candidate" (required Parser.distribution)
        ~doc:"distribution Candidate timeout Statistical Distribution, this
        gives the distrubut"
     +> flag "-leader" (required Parser.distribution)
        ~doc:"distribution Leader Statistical Distribution"
     +> flag "-delay" (required Parser.distribution)
        ~doc:"distribution Packet Delay Statistical Distribution"
     +> flag "-failure" (optional Parser.distribution)
        ~doc:"distribution Node failure Statistical Distribution"
     +> flag "-recover" (optional Parser.distribution)
        ~doc:"distribution Node recovery Statistical Distribution"
     +> flag "-termOnElec" no_arg
         ~doc:"Terminate when a leader has successfully been established"
      +> flag "-termOnClient" no_arg
         ~doc:"Terminate when a client workload is empty"
      +> flag "-cmds" (optional_with_default 5 int)
          ~doc:"Size of test workload"
      +> flag "-clientWait" (optional_with_default 5 int)
        ~doc:"Time a client waits between requests"

 )

let realtime =
  Command.basic
    ~summary:"Realtime Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see www.cl.cam.ac.uk/~hh360 for more information ")
  Command.Spec.(
    empty
     ++ common ()
      )
    (fun nodes term debug_enabled iter data follower candidate leader delay failure recover term_ele term_client cmds wait () ->  
      printf "%s" (Parser.run ~time:Real ~nodes ~term ~debug_enabled ~iter ~data ~follower ~candidate ~leader ~delay ~failure ~recover ~term_ele ~term_client ~cmds ~wait)) 

let discrete =
  Command.basic
    ~summary:"Discrete Event Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see www.cl.cam.ac.uk/~hh360 for more information ")
  Command.Spec.(
    empty
     ++ common ()
      )
    (fun nodes term debug_enabled iter data follower candidate leader delay failure recover term_ele term_client cmds wait () ->  
      printf "%s" (Parser.run ~time:Discrete ~nodes ~term ~debug_enabled ~iter ~data ~follower ~candidate ~leader ~delay ~failure ~recover ~term_ele ~term_client ~cmds ~wait))

let () =  
  ["realtime",realtime;"discrete",discrete]
  |> Command.group ~summary:"Discrete Event Simulator & Realtime Simulator for
  Raft's Leader Election"
  |> Command.run
