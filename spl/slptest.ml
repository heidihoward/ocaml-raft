open Core.Std
open Raftconsensus

let apply x y = tick y x

let () = 
  let monitor = init () in
  apply `Startup monitor
  |> apply `StartElection
  |> apply `WinElection
  |> apply `RestartElection
  |> (fun _ -> printf "done")
  
