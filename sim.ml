open Core.Std
open Core_extended.Std (* used for Shell scripting, could be replaced with async shell *)
open Async.Std
open Msg

let nodes = Array.create ~len:10 None (* a much better data structure for nodes is needed *)
let log_w = Msg.msg_log "simulator" 

(* given a msg, forward to node, in future add delays & randomly drop msgs *)
let fwd_msg msg = 
	let msg = String.split msg ~on:':' in 
	(match msg with 
	| too::from::msg::_ ->  printf "message to %s from %s saying %s \n %!" too from msg ;
				(match nodes.(int_of_string too) with
				| Some (_,_,_,w) -> Writer.write_line w (too^":"^from^":"^msg);  printf "ok, message sent  \n %!" 
				| None -> printf "trying to send msg to client before they have connected \n %! ")
	| _ -> printf "simulator has recieved msg of wrong format")

(* handles incoming msgs and fowards them on*)
let rec msg_rcv r = 
 Reader.read_line r 
  >>|(function | `Ok msg -> Msg.append_log log_w msg; fwd_msg msg; ignore(msg_rcv r) 
	       | `Eof -> ignore(msg_rcv r) )  
 
(* handler called for each node that connect over tcp *)				
let handler address r w = 
  (*handle hello msg *)
  Reader.read_line r 
  >>| ( function  
    | `Ok msg -> 
      let msg = String.split msg ~on:':' in
      (match msg with 
        | "SIM"::id::"hello"::_ -> 
				let id = int_of_string id in
				nodes.(id)  <- Some (id,address,r,w) ;
				printf "connected to %x \n %!" id;
				Writer.write_line w "your connected";
	| _ -> printf "unsuccessfull connection \n %!") 
    | _ -> printf "unsuccessfull connection \n %!" ) 
  (* handle all other msgs *)
  >>= (fun _ -> msg_rcv r )

let run ~nodes =
  (* starting tcp server *)
  (printf "starting tcp server \n %!" ;
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

