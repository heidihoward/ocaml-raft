open Core.Std
open Async.Std

let hello_spec = Rpc.Rpc.create 
  ~name:"HelloMsg"
  ~version:0
  ~bin_query:String.bin_t
  ~bin_response:String.bin_t

let hello_impl () msg = printf "%s \n" msg; return "hello to you too"

let start_listening ~port = 
    let implementations = Rpc.Implementations.create_exn 
    ~implementations:[Rpc.Rpc.implement hello_spec hello_impl ] 
    ~on_unknown_rpc: `Ignore in
  Rpc.Connection.serve ~implementations
  ~initial_connection_state:(fun _ -> ()) ~where_to_listen:(Tcp.on_port port) ()

let dispatch_hello port = 
    Tcp.with_connection (Tcp.to_host_and_port "localhost" port) (fun _ r w ->
    Rpc.Connection.create ~connection_state:() r w
    >>= function
    | Error exn -> raise exn
    | Ok conn -> 
       printf "Dispatching hello RPC";
       Rpc.Rpc.dispatch hello_spec conn ("hello from "^(Int.to_string port) ))
  

let run ~port ~friends = 
  start_listening ~port
  >>= (fun _ -> after (Time.Span.create ~sec:5 () ))
  >>= (fun () -> Deferred.all (List.map ~f:dispatch_hello friends))
  >>= (fun _ -> Deferred.never ()) 


let () = 
  (Command.async_basic
  ~summary:"real simulator"
  ~readme:(fun () -> ":)")
  Command.Spec.(
     empty 
    +> flag "-port" (optional_with_default 8888 int)
      ~doc:"port to listen on"
    +> flag "-friends" (listed int)
      ~doc:"port to communicate with"
    )
  (fun port friends () -> run ~port ~friends) )
    |> Command.run 
