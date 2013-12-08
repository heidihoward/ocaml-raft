open Core.Std
open Async.Std

let state :State.t  = State.init()

let debug_active = ref true

let debug x = if !debug_active then 
  let time = Time.Ofday.to_sec_string (Time.Ofday.now()) in
  let id = State.get_id state in
  (printf "[ID:%i %s] %s \n %!" id time x)

(*___RPC Specification and Implementations___*)

type raftrpc = RequestVote | AppendEntries

module RequestVoteArg = struct
  type t = { term: int;
             id: int;
             lastlogindex: int;
             lastlogterm: int; }
  with bin_io
end

module RequestVoteRes = struct
  type t = { term: int;
             votegranted: bool}
  with bin_io
end

module AppendEntriesArg = struct
  type t = { term: int;
             leaderid: int;
             prevlogindex: int;
             prevlogterm: int;
             entries: Lg.t;
             commitindex: int}
  with bin_io
end 

module AppendEntriesRes = struct
  type t = { term: int;
             success: bool }
  with bin_io
end

(*Specification for the RequestVote RPC *)
let requestvote_spec = Rpc.Rpc.create
  ~name:"RequestVote"
  ~version:0
  ~bin_query:RequestVoteArg.bin_t
  ~bin_response:RequestVoteRes.bin_t

(*Specification of the AppendEntries RPC *)
let appendentries_spec = Rpc.Rpc.create
  ~name:"AppendEntries"
  ~version:0
  ~bin_query:AppendEntriesArg.bin_t
  ~bin_response:AppendEntriesRes.bin_t

(*Implementation for the RequestVote RPC *)
let requestvote_impl () args = 
  debug "Incoming RequestVote RPC";
  
  return RequestVoteRes.({term=2; votegranted=true})

(*Implementation for the AppendEntries RPC *)
let appendentries_impl () x = 
  debug "Incoming AppendEntries RPC";
  return AppendEntriesRes.({term=2; success=true})


(*___Role Transitions__ *)

let rec candidate () =
        debug "Candidate mode";
  (* TODO: Implement candidate 
  let timeout =  Time.Span.create ~sec:10 ()PCn
  State.trans state Candidate;
  State.self_vote state;
  Deferred.any 
  [Clock.after timeout >>> (fun _ -> State.election_timeout state) ;
  List.iter (State.get_addr_all state) ~f:(fun (_,(host,port)) -> dispatch requestvote_spec host port) ]
  match State.election_status state with
    | None ->
    | Timeout -> candidate()
    | Step_up -> 
    | Step_down -> *)
  Deferred.never()

let follower () = 
 debug "Follower mode";
 (* TODO: Implement follower *)
  Deferred.never()

let leader () = 
 debug "Leader mode";
 (* TODO: Implement leader *)
  Deferred.never()

(* Connect to TCP server at port [peer] and dispatch myrpc request *)
let dispatch_requestvote host port =
  ignore (
    Tcp.with_connection (Tcp.to_host_and_port host port) (fun _ r w ->
    Rpc.Connection.create ~connection_state:() r w
    >>= function
    | Error exn -> raise exn
    | Ok conn -> 
       debug "Dispatching RequestVote RPC";
       Rpc.Rpc.dispatch requestvote_spec conn 
    RequestVoteArg.({
      term= State.get_term state;
      id= State.get_id state;
      lastlogindex= State.get_lastlogindex state;
      lastlogterm= State.get_lastlogterm state; })
    ))

(*Start a RPC server on [port] *)
let start_server ~port =
  let implementations = Rpc.Implementations.create_exn 
    ~implementations:[Rpc.Rpc.implement requestvote_spec requestvote_impl; 
                      Rpc.Rpc.implement appendentries_spec appendentries_impl ] 
    ~on_unknown_rpc: `Ignore in
  Rpc.Connection.serve ~implementations
  ~initial_connection_state:(fun _ -> ()) ~where_to_listen:(Tcp.on_port port) ()

let run ~id ~port ~peerid ~peerip ~peerport =
  State.set_id state id; 
  debug ("launching "^(Int.to_string id));
  let nodes = List.zip_exn peerid (List.zip_exn peerip peerport)  in
  State.add_nodes state nodes;
  List.iter (State.get_addr_all state) ~f:(fun (_,(host,port)) -> dispatch_requestvote host port) ;
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
