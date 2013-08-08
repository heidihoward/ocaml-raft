open Core.Std
open Core_extended.Std (* used for Shell scripting, could be replaced with async shell *)
open Async.Std
open Msg

let nodes = Array.create ~len:4 None 
let log_w = Msg.msg_log "simulator" 

let fwd_msg msg = 
	let msg = String.split msg ~on:':' in 
	(match msg with 
	| too::from::msg::_ ->  printf "message to %s from %s saying %s \n" too from msg ;
				(match nodes.(int_of_string too) with
				| Some (_,_,_,w) ->  printf "ok"; Writer.write_line w (too^":"^from^":"^msg) 
				| Some _ -> printf "oooo" 
				| None -> printf "double oooo"))

let rec msg_rcv r = 
 Reader.read_line r 
  >>|(function | `Ok msg -> Msg.append_log log_w msg; fwd_msg msg; ignore(msg_rcv r) 
	       | `Eof -> ignore(msg_rcv r) )  
 
(* handler called for each node that connect over tcp *)				
let handler address r w = 
  Reader.read_line r 
  >>| ( function  
    | `Ok msg -> 
      let msg = String.split msg ~on:':' in
      (match msg with 
        | "SIM"::id::"hello"::_ ->  msg_rcv r ;
				let id = int_of_string id in
				nodes.(id)  <- Some (id,address,r,w) ;
				printf "connected to %x \n %!" id;
				Msg.append_log log_w "connected"
	| _ -> printf "unsuccessfull %!") 
    | _ -> printf "unsuccessfull %!" ) 

let run ~nodes =
  (* starting tcp server *)
  (printf "starting tcp server %!" ;
  let host_and_port = Tcp.Server.create (Tcp.on_port 8889) handler in
  host_and_port
  (* starting nodes *)
  >>| (fun _ -> 
    for i=0 to nodes-1 do 
      let id = (string_of_int i) in
      Shell.run "./node.byte" ["-id";id];
      printf "starting up node %x \n %!" i 
    done ));
    Deferred.never ()

(*command line parsing & starting async schedular *)
 let () =
  Command.async_basic ~summary:"Start an a consensus simulator"
    Command.Spec.(
      empty
      +> flag "-nodes" (required int) ~doc:" # of nodes to simulate" )
    (fun nodes () -> run ~nodes)
  |> Command.run 

(* let () =
  run ~nodes:4 ;
  never_returns (Scheduler.go ())  *)

