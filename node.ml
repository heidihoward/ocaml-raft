open Core.Std
open Async.Std

(*Specification for the RPC method called myrpc *)
let myrpc_spec = Rpc.Rpc.create
  ~name:"my-little-rpc"
  ~version:0
  ~bin_query:String.bin_t
  ~bin_response:String.bin_t

(*Implementation for the RPC method called myrpc *)
let myrpc_impl () x = printf x; return "hello World"

(* Connect to TCP server at port [peer] and dispatch myrpc request *)
let start_client peer =
  Tcp.with_connection (Tcp.to_host_and_port "127.0.0.1" peer) (fun _ r w ->
    Rpc.Connection.create ~connection_state:() r w
    >>= function
    | Error exn -> raise exn
    | Ok conn -> Rpc.Rpc.dispatch myrpc_spec conn "good morning jon" )

(*Start a RPC server on [port] *)
let start_server ~port =
  let implementations = Rpc.Implementations.create 
    ~implementations:[Rpc.Rpc.implement myrpc_spec myrpc_impl ] 
    ~on_unknown_rpc: `Ignore in
  Rpc.connection.serve ~implementations ~initial_connection_state:() (Tcp.on_port port) 

let run ~port ~peers = 
  printf "hello";
  start_server ~port;
  List.iteri peers start_client 

let () =
  Command.async_basic
    ~summary:"Start an network node"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 8888 int)
        ~doc:" Port to listen on (default 8888)"
      +> flag "-peers" (requried listed int)
        ~doc:" Peers to say hello to"

    )
    (fun port peers () -> run ~port ~peers)
  |> Command.run
