open Core.Std
open Common
open MonoTime
open Output
open Event
module Client = Client.ClientHandler

type networksig = float * (unit -> float) * (unit -> unit)
type eventsig = State.t -> (State.t * EventList.item list)
type clientsig = Client.t -> (Client.t * EventList.item list)

let unicast_replica (loss,delay,incr) (dist: IntID.t) (t:MonoTime.t) (e) = 
  (*TODO: modify these to allow the user to specify some deley
   * distribution/bound *)
 if (NumberGen.to_drop loss) then 
  (debug "packet dropped"; Ignore t)
 else 
  (
  let delay = MonoTime.span_of_float (delay()) in
  let arriv = MonoTime.add t delay in
  debug ("dispatching msg to "^(IntID.to_string dist) ^ " to arrive at "^
  (MonoTime.to_string arriv));
  json ~start:false (`Assoc [
    ("arrives", `Int (MonoTime.to_int arriv));
    ("sent", `Int (MonoTime.to_int t));
    ("dest", `Int (IntID.to_int dist))
  ]);
  incr ();
  RaftEvent (arriv, dist, e) 
  )

let unicast_client (loss, delay, incr) (t:MonoTime.t) (e) =
  if (NumberGen.to_drop loss) then 
  (debug "packet dropped"; Ignore t)
 else (
  let delay = MonoTime.span_of_float (delay()) in
  let arriv = MonoTime.add t delay in
  debug ("dispatching msg to client to arrive at "^
  (MonoTime.to_string arriv));
  incr ();
  ClientEvent (arriv, e))

let broadcast net (dests:IntID.t list) (t:MonoTime.t) e  = 
  List.map dests ~f:(fun dst -> unicast_replica net dst t e) 