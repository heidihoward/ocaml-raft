open Core.Std
open Common


let common () =
    Command.Spec.(
      empty
      +> flag "-nodes" (required int) 
        ~doc:"int Number of nodes to simulate, must be 2 or greater"
      +> flag "-eligible" (optional int)
        ~doc:"int Number of simulated nodes which are eligible for leadership, default to all"
      +> flag "-termOnTimeout" (optional_with_default 5000 int)
        ~doc:"int The maxiumun number of terms before termination"
      +> flag "-d" no_arg
        ~doc:"Enable debugging output (disabled by default)"
      +> flag "-json" no_arg
        ~doc:"Enable Json Output (disabled by default)"
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
      +> flag "-clientWaitSuccess" (optional_with_default 0 int)
        ~doc:"Time a client waits after a successful requests"
      +> flag "-clientWaitFailure" (optional_with_default 0 int)
        ~doc:"Time a client waits after a failed requests"        
      +> flag "-clientTimeout" (optional_with_default 100 int)
        ~doc:"Timeout that a client waits for the response from the cluster"  
      +> flag "-backoff" no_arg
        ~doc:"Enable the binary exponential for candidates with majority rejections" 
      +> flag "-loss" (optional_with_default 0.0 float)
        ~doc:"Probabilty that a packet will be lossed"
      +> flag "-hist" no_arg
        ~doc:"Enable storage of simulation trace"
      +> flag "-conservative" no_arg
        ~doc:"Enable conservative use of AppendEntries so they only go out on ehop"
 )

let realtime =
  Command.basic
    ~summary:"Realtime Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see github.com/heidi-ann/ocaml-raft for more information ")
  Command.Spec.(
    empty
     ++ common ()
      )
    (fun nodes eligible term debug_enabled json_enabled iter data follower candidate leader delay failure recover term_ele 
        term_client cmds wait_succ wait_fail timeout_client backoff loss hist cons () ->  
      printf "%s" (Parser.run ~time:Real ~nodes ~eligible ~term ~debug_enabled ~json_enabled ~iter ~data ~follower ~candidate ~leader ~delay 
      ~failure ~recover ~term_ele ~term_client ~cmds ~wait_succ ~wait_fail ~timeout_client ~backoff ~loss ~hist ~cons )) 

let discrete =
  Command.basic
    ~summary:"Discrete Event Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see github.com/heidi-ann/ocaml-raft for more information ")
  Command.Spec.(
    empty
     ++ common ()
      )
    (fun nodes eligible term debug_enabled json_enabled iter data follower candidate leader delay failure recover term_ele 
        term_client cmds wait_succ wait_fail timeout_client backoff loss hist cons () ->  
      printf "%s" (Parser.run ~time:Discrete ~nodes ~eligible ~term ~debug_enabled ~json_enabled ~iter ~data ~follower ~candidate ~leader ~delay 
      ~failure ~recover ~term_ele ~term_client ~cmds ~wait_succ ~wait_fail ~timeout_client ~backoff ~loss ~hist ~cons ))

let () =  
  ["realtime",realtime;"discrete",discrete]
  |> Command.group ~summary:"Discrete Event Simulator & Realtime Simulator for
  Raft's Leader Election"
  |> Command.run
