open Core.Std
open Common
open Async.Std

let msg_protocol = Rpc.Rpc.create 
  ~name:"HelloMsg"
  ~version:0
  ~bin_query:String.bin_t
  ~bin_response:String.bin_t

let run ~port = 


let parse  = Command.basic
  ~summary:"real simulator"
  ~readme:(fun () -> ":)"
  Command.Spec.(
    +> empty 
    +> flag "-port" (optional_with_default 8888 int)
      ~doc:"port to listen on"
    )
  (fun port () -> run ~port) 

let () = Command.run parse
