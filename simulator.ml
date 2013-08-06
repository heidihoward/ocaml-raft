open Core.Std
open Core_extended.Std
open Async.Std

let handler address r w =
  Reader.read_line r 
  >>| (function | `Ok "hello" -> printf "successfull"
		| `Eof -> printf "unsuccessfull")

let run () =
  let host_and_port =
    Tcp.Server.create (Tcp.on_port 8888) handler
  in
  host_and_port
  >>> (fun _ -> Shell.run "./server.native" [])

let () =
  run ();
  never_returns (Scheduler.go ())
