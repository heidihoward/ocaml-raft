open Core_kernel.Std
open Common
open State
module Client = Client.ClientHandler

type item = (MonoTime.t,IntID.t,State.t,Client.t) Event.t
type t = item list

let hd el = match el with
  | [] -> None 
  | x::xs -> Some(x,xs)

let add a l = 
  List.merge l (List.sort a ~cmp:Event.compare ) ~cmp:Event.compare

let init init_events = add init_events []

