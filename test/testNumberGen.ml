open Core_kernel.Std
open Async.Std
open Common
  
let run n f = 
  Writer.open_file ("test"^(string_of_int n)^".data") 
  >>| (fun wrt -> for i=0 to 2000 do 
    f () 
    |> Float.to_string
    |> Writer.write_line wrt done)

let tests = 
  let open NumberGen in
  [uniform_float 0.0 10.0; exp_float 10.0; exp_float 20.0]

let () = 
  ignore(List.mapi tests ~f:run); never_returns (Scheduler.go ())
