open Core.Std
open Async.Std

let state :State.t  = State.empty

(*Specification for the RPC method called myrpc *)
let myrpc_spec = Rpc.Rpc.create
  ~name:"my-little-rpc"
  ~version:0
  ~bin_query:String.bin_t
  ~bin_response:String.bin_t

(*Implementation for the RPC method called myrpc *)
let myrpc_impl () x = printf "%s"x; return "hello World"

(* Connect to TCP server at port [peer] and dispatch myrpc request *)
let start_client host port =
  ignore (
    Tcp.with_connection (Tcp.to_host_and_port host port) (fun _ r w ->
    Rpc.Connection.create ~connection_state:() r w
    >>= function
    | Error exn -> raise exn
    | Ok conn -> Rpc.Rpc.dispatch myrpc_spec conn "good morning jon"))

(*Start a RPC server on [port] *)
let start_server ~port =
  let implementations = Rpc.Implementations.create_exn 
    ~implementations:[Rpc.Rpc.implement myrpc_spec myrpc_impl ] 
    ~on_unknown_rpc: `Ignore in
  Rpc.Connection.serve ~implementations
  ~initial_connection_state:(fun _ -> ()) ~where_to_listen:(Tcp.on_port port) ()

let run ~id ~port ~peerid ~peerip ~peerport =
  State.set_id state id; 
  printf "hello from %i" (State.get_id state);
  let nodes = List.zip_exn peerid (List.zip_exn peerip peerport)  in
  State.add_nodes state nodes;
  List.iter (State.get_addr_all state) ~f:(fun (_,(host,port)) -> start_client host port) ;
  ignore (start_server ~port);
  Deferred.never()


let () =
  Command.async_basic
    ~summary:"Start an network node"
    Command.Spec.(
      empty
      +> flag "-id" (required int)
        ~doc:" candidate ID"
      +> flag "-port" (optional_with_default 8888 int)
        ~doc:" Port to listen on (default 8888)"
      +> flag "-peerid" (listed int)
        ~doc:" Peers to say hello to"
      +> flag "-peerip" (listed string)
        ~doc:" IP addresses of peers (same order as ID's)"
      +> flag "-peerport" (listed int)
        ~doc:" ports of peers (same order as ID's)"
    )
    (fun id port peerid peerip peerport () -> run ~id ~port ~peerid ~peerip ~peerport)
  |> Command.run
