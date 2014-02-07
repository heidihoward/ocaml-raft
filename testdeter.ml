open Core.Std
open Async.Std
open Common
  
let run_test (n:int) (f:unit -> string) = 
  Writer.open_file ("test"^(string_of_int n)^".data") 
  >>| 
  (fun wrt -> for i=0 to 10 do 
     let result = f () in 
      printf "test number: %i, iteration: %i, result: %s \n !" n i result;
      Writer.write_line wrt result
  done; wrt)
  >>= Writer.close

  let run_test2 n f = printf "test result %s" f

let () = 
  let timeout_test (min,max) = printf "%s" (
    Parser.run ~time:Real ~nodes:4 ~term:50000 ~debug_enabled:true ~iter:1 ~data:None 
    ~follower:(NumberGen.string_to_dist ("Uniform-"^(Int.to_string min)^"-"^(Int.to_string max))) 
    ~candidate:(NumberGen.string_to_dist "Fixed-50") 
    ~leader:(NumberGen.string_to_dist "Fixed-50") 
    ~delay:(NumberGen.string_to_dist "Fixed-6")  
    ~failure:None 
    ~recover:None )in
  ignore (List.map [(12,24);(25,50);(50,100);(100,200);(150,300)] ~f:timeout_test)

(*
let () = 
  ignore(List.mapi tests ~f:run_test2) (*  ; never_returns (Scheduler.go ()) *) *)
