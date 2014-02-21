open Core.Std
open Common

(** [EVENTLIST] is a datastructure for holding Events, there thing with a compare operator *)
module type EVENTLIST =
  sig
    type item
    type t
    (** given an eventlist, hd returns either nothing or the top item and the next eventlist data structure *)
    val hd : t -> (item * t) option 
    (** given a list of new events and a eventlist, add returns a new eventlist *)
    val add : item list -> t -> t
    (** given a list of events, init generates an eventlist *)
    val init : item list -> t
  end

module LinkedList (E: sig type t with compare end) = struct

  type item = E.t
  type t = item list

  let hd el = match el with
    | [] -> None 
    | x::xs -> Some(x,xs)

  let add a l = 
    List.merge l (List.sort a ~cmp:E.compare) ~cmp:E.compare

  let init init_events = add init_events []

end

