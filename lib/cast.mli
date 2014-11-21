open Common

(*Setting up the StateList and State, which hold the state for the nodes *)
module Client = Client.ClientHandler

type networksig = float * (unit -> float) * (unit -> unit)
type eventsig = State.t -> (State.t * EventList.item list)
type clientsig = Client.t -> (Client.t * EventList.item list)

val unicast_replica: networksig -> IntID.t -> MonoTime.t -> eventsig -> EventList.item
val unicast_client: networksig -> MonoTime.t -> clientsig -> EventList.item
val broadcast: networksig -> IntID.t list -> MonoTime.t -> eventsig -> EventList.item list
