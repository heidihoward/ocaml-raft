open Core_kernel.Std
open OUnit
open RaftMonitorWrapper

let apply x y = tick y x

let test_general2 _ = 
  let monitor = init () in
  apply `Startup monitor
  |> apply `StartElection
  |> apply `WinElection
  |> apply `RestartElection
  |> (fun _ -> ())

let test_general1 _ = 
  let monitor = init () in
  apply `Recover monitor
  |> apply `StartElection
  |> apply `RestartElection 
  |> apply `RestartElection
  |> apply `WinElection
  |> apply `StepDown_from_Leader
  |> (fun _ -> () )


let suite = "SPL Test" >::: 
	["general_succcessful" >:: test_general1;
     "general_failure" >:: test_general2;
     ]

 let _ =
   run_test_tt_main suite
