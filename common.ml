open Core.Std

let debug_active = ref true

let debug x = if !debug_active then
    (printf " %s \n"  x)


module type PARAMETERS = sig
  val timeout: unit -> int
  val nodes: int
  val pkt_delay: unit -> int
end

module type INDEX = sig
  type t
  val succ: t -> t
  val init: unit -> t
  val to_string: t -> string
end 

module Index : INDEX = struct
  type t = int with compare
  let succ = succ
  let init () = 0
  let to_string = string_of_int
end

module type ID = sig
  type t
(*  type loc*)
(*  type msg *)
  val from_int: int -> t
  val to_int: t -> int
  val comp: t -> t -> bool
  val to_string: t -> string
(*  val get_loc: t -> loc *)
(*  val set_loc: t -> loc -> t *)
(*  val dispatch: t -> msg -> t
  val collect: t -> msg list *)
end 

module IntID : ID  = struct
  type t = int
(*  type loc = unit *)
(*  type msg = unit *)
  let from_int x = x
  let to_int x  = x
  let comp t1 t2 = phys_equal t1 t2
  let to_string = string_of_int
(*  let get_loc _ = None *)
(*  let set_loc t _ = t *)(* simple IntID don't hold any location information *) 
(*  let dispatch x _ = x
  let collect _ = [] *)
end  

module type ENTRY = sig 
  type t 
  val to_string: t -> string
end 

module LogEntry: ENTRY = struct
  (*TODO copy over proper application from other .mls *)
  type t = A | B | C
  let to_string = function
    | A -> "A"
    | B -> "B"
    | C -> "C"
end


module type LOG = functor (E: ENTRY) -> sig
  type t
  val init: unit -> t
  val append: t -> E.t -> t
  val to_string: t -> string
end

module ListLog : LOG =
  functor (LogEntry: ENTRY) ->  struct
  type t = LogEntry.t list
  let init () = []
  let append t x = x::t
  let to_string = List.to_string ~f:LogEntry.to_string
end

module Event = struct 
  type ('a,'b,'c) t = E of ('a * 'b * ('a,'b,'c) event)
  and ('a,'b,'c) event = ('c -> ('c * ('a,'b,'c) t list))

  let compare x y = match x,y with
  | (E (xt,_,_),E (yt,_,_)) -> compare xt yt 
  
 (* type ('a,'b,'c) t = E of ('a * ('a,'b,'c) event)
  and ('a,'b,'c) event = ('b -> ('b * ('c,'b) t list * ('a,'b) t list))

  let compare x y = match x,y with
  | (E (xt,xe),E (yt,ye)) -> compare xt yt *)
end 

module type EVENTLIST = sig
  type ('a,'b,'c) t
  val from_list: ('a,'b,'c) Event.t list -> ('a,'b,'c) t
  val to_list: ('a,'b,'c) t -> ('a,'b,'c) Event.t list
  val hd: ('a,'b,'c) t -> ('a,'b,'c) Event.t option
  val add: ('a,'b,'c) Event.t -> ('a,'b,'c) t -> ('a,'b,'c) t
end

module EventList = struct

  type ('a,'b,'c) t = ('a,'b,'c) Event.t  list

  let from_list x = List.sort ~cmp:Event.compare x
  let to_list x = x

(*  let find x l =  
    let open Event in
    let f = function | E (t,e) -> (x=t) in
    (* TODO write better implementation of find which returns list *)
    match (List.partition_tf l ~f) with
  | (E (_,e)::_,ls) -> Some (e,ls)
  | ([],_) -> None  *)

  let hd = function
    | [] -> None 
    | x::xs -> Some(x,xs)

  let add a l = 
    List.merge l (from_list a) ~cmp:Event.compare
end


