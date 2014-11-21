open Core.Std
open Common
module Client = Client.ClientHandler

type item = (MonoTime.t,IntID.t,State.t,Client.t) Event.t
type t = item list

val hd: t -> (item * t) option
(** given a list of new events and a eventlist, add returns a new eventlist *)
val add : item list -> t -> t
(** given a list of events, init generates an eventlist *)
val init : item list -> t