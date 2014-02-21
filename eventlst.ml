open Core.Std
open Common

(** [EVENTLIST] is a datastructure for holding Events, there thing with a compare operator *)
module type EVENTLIST =
  sig
    type ('a, 'b, 'c) t
    (** given an eventlist, hd returns either nothing or the top event and the next eventlist data structure *)
    val hd : ('a, 'b, 'c) t -> (('a, 'b, 'c) Event.t * (('a, 'b, 'c) t)) option 
    (** given a list of new events and a eventlist, add returns a new eventlist *)
    val add : ('a, 'b, 'c) Event.t list -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
    (** given a list of events, init generates an eventlist *)
    val init : ('a, 'b, 'c) Event.t list -> ('a, 'b, 'c) t
  end

module LinkedList : EVENTLIST = struct

  type ('a,'b,'c) t = ('a,'b,'c) Event.t list

  let hd el = match el with
    | [] -> None 
    | x::xs -> Some(x,xs)

  let add a l = 
    List.merge l (List.sort a ~cmp:Event.compare) ~cmp:Event.compare

  let init init_events = add init_events []

end
