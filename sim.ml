open Core.Std
open Core_extended.Std (* used for Shell scripting, could be replaced with async shell *)
open Async_shell 
open Async.Std
open Msg

let nodes = Array.create ~len:10 None (* a much better data structure for nodes is needed *)
let log_w = Msg.msg_log "simulator" 

let debug_active = ref true

let debug x = if !debug_active then 
  let time = Time.Ofday.to_sec_string (Time.Ofday.now()) in
  (printf "[%s] %s \n %!" time x)


let msg_snd pkt_w log_w too msg =
  let msg_pkt = (too^":SIM:"^msg) in
  Msg.append_log log_w msg_pkt; 
  Writer.write_line pkt_w msg_pkt

(* given a msg, forward to node, in future add delays & randomly drop msgs *)
let fwd_msg log_w msg = 
	let msg = String.split msg ~on:':' in 
	(match msg with 
	| too::from::msg::_ ->  debug ("TO: "^too^" FROM: "^from^" MSG: "^msg);
				(match nodes.(int_of_string too) with
				| Some (_,_,_,pkt_w) -> msg_snd pkt_w log_w too msg ;  debug "ok, message sent" 
				| None -> debug "ERROR trying to send msg to client before they have connected")
	| _ -> debug "simulator has recieved msg of wrong format")

(* handles incoming msgs and fowards them on*)
let rec msg_rcv r = 
 Reader.read_line r 
  >>> (function | `Ok msg -> fwd_msg log_w msg; msg_rcv r 
	        | `Eof -> msg_rcv r )  
 
(* handler called for each node that connect over tcp *)				
let handler address r w = 
  (*handle hello msg *)
  Reader.read_line r 
  >>| ( function  
    | `Ok msg -> 
      let msg = String.split msg ~on:':' in
      (match msg with 
        | "SIM"::from::"hello"::_ -> 
				let id = int_of_string from in
				nodes.(id)  <- Some (id,address,r,w) ;
				debug ("connected to "^from);
				Writer.write_line w "your connected";
	| _ -> debug "unsuccessfull connection \n %!") 
    | _ -> debug "unsuccessfull connection \n %!" ) 
  (* handle all other msgs *)
  >>| (fun _ -> msg_rcv r )

let run ~nodes ~debugon =
  (* enable/disable debugging *)
  debug_active := debugon;
  (* starting tcp server *)
  (debug "starting tcp server" ;
  let timephase = Time.Span.create ~sec:2 () in
  let host_and_port = Tcp.Server.create (Tcp.on_port 8889) handler in
  host_and_port
  (* starting nodes *)
  >>= (fun _ -> Clock.after timephase)
  >>> (fun _ -> 
    if (nodes > 0) then
      for i=0 to nodes-1 do 
        let id = (string_of_int i) in
        ignore (Async_shell.run "./node.byte" ["-id";id]);
        debug ("starting up node ID:"^id)
      done )  );
    Deferred.never ()

(*command line parsing & starting async schedular *)
 let () =
  Command.async_basic ~summary:"Start an a P2P network simulator"
    Command.Spec.(
      empty
      +> flag "-nodes" (optional_with_default 0 int) 
      ~doc:" # of automatically launched nodes to simulate, cant be greater than 9" 
      +> flag "-debugon" (no_arg) 
      ~doc:" add to enabled debuging" )
    (fun nodes debugon () -> run ~nodes ~debugon)
  |> Command.run 

(* let () =
  run ~nodes:4 ;
  never_returns (Scheduler.go ())  *)

