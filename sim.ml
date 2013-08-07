open Core.Std
open Core_extended.Std
open Async.Std

let nodes = Array.create ~len:3 None

let fwd_msg msg = 
	let msg = String.split msg ~on:':' in 
	(match msg with 
	| too::from::msg::_ ->  printf "message to %s from %s saying %s \n" too from msg ;
				(match nodes.(int_of_string too) with
				| Some (_,_,_,w) -> Writer.write_line w (too^":"^from^":"^msg) 
				| Some _ -> printf "oooo" 
				| None -> printf "double oooo"))

let rec msg_rcv r = 
 Reader.read_line r 
  >>|(function | `Ok msg -> fwd_msg msg; ignore(msg_rcv r) 
	       | `Eof -> ignore(msg_rcv r) ) 
 
				
 
 let handler address r w =
  Reader.read_line r 
>>|(function | `Ok msg -> 
		let msg = String.split msg ~on:':' in
		( match msg with 
		  | "SIM"::id::"hello"::_ -> msg_rcv r ; 
					     let id = int_of_string id in
					     nodes.(id)  <- Some (id,address,r,w) ;
					     printf "connected to %x \n" id)
	       | `Eof -> printf "unsuccessfull" )



let run () =
  let host_and_port =
    Tcp.Server.create (Tcp.on_port 8888) handler
  in
  host_and_port
  >>> (fun _ -> Shell.run "./server.native" ["-id";"0"]; Shell.run "./server.native" ["-id";"1"]; Shell.run "./server.native" ["-id";"2"]) 

let () =
  run ();
  never_returns (Scheduler.go ())
