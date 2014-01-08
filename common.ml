open Core.Std
open Async.Std
(* This module contains basic functions and modules used through, with no
 * external dependances on any other modules *)

let debug x = (*if !debug_active then*)
    (printf " %s \n"  x)

type role = Follower | Candidate | Leader with sexp
type value = Discrete of int | Continous of float
type 'a status = Live of 'a | Down of 'a | Notfound 
type failures = Wake | Kill

let string_of_role = function
  | Follower -> "Follower"
  | Candidate -> "Candidate"
  | Leader -> "Leader"

(* module type STATELIST = sig
  (* STATELIST is essentially a wrapper around List.Assoc to manage simulated node failures *)
  type ('id,'state) t
  val find: ('id,'state) t -> 'id -> 'state status
  val add: ('id,'state) t -> 'id -> 'state status -> ('id,'state) t
  val check_condition: ('id,'state) t -> f:(('id * ('state status)) -> bool) -> bool
  val from_listassoc: ('id, 'state status ) List.Assoc.t -> ('id,'state) t
  val kill: ('id, 'state) t -> 'id -> ('id,'state) t
  val wake: ('id, 'state) t -> 'id -> ('id,'state) t
end

module StateList : STATELIST = struct
  type ('id,'state) t = ('id,'state status) List.Assoc.t
  let find sl id  = match (List.Assoc.find sl id) with
    | Some x -> x | None -> Notfound

  let add sl id state = List.Assoc.add sl id state 
  let from_listassoc x = x

  let check_condition sl ~f = 
    match (List.find sl ~f) with Some _ -> true | None -> false 

  let kill sl id = 
    match (find sl id) with
    | Live s -> List.Assoc.add sl id (Down s)

  let wake sl id =
    match (find sl id) with 
    | Down s -> List.Assoc.add sl id (Live s)

end
*)

module NumberGen = struct
  (* TODO: these are dealing with discite values but i think i need a seperate
   * ones for continous *)
  let () = Random.self_init ()
  let fixed x () = x
  let uniform_int min max () = Random.int (max-min) + min
  let uniform_float min max () = Random.float (max-.min) +. min
  let exp_float lam () = (-1.0 /. lam)*.log(Random.float Float.max_finite_value)
end

module type PARAMETERS = sig
  val timeout: role -> value
  val nodes: int
  val pkt_delay: unit -> value
  val termination: int
  val debug_mode: string -> unit
  val nxt_failure: unit -> value
  val nxt_recover: unit -> value
end

module type INDEX = sig
  type t with sexp,bin_io
  val succ: t -> t
  val init: unit -> t
  val to_string: t -> string
end 

module Index : INDEX = struct
  type t = int with compare,sexp,bin_io
  let succ = succ
  let init () = 0
  let to_string = string_of_int
end

module type NODE_ID = sig
  type t with sexp,bin_io
(*  type loc*)
(*  type msg *)
  val from_int: int -> t
  val to_int: t -> int
  val equal: t -> t -> bool
  val to_string: t -> string
(*  val get_loc: t -> loc *)
(*  val set_loc: t -> loc -> t *)
(*  val dispatch: t -> msg -> t
  val collect: t -> msg list *)
end 

module IntID : NODE_ID  = struct
  type t = int with sexp,bin_io
(*  type loc = unit *)
(*  type msg = unit *)
  let from_int x = x
  let to_int x  = x
  let equal = Int.equal
  let to_string = string_of_int
(*  let get_loc _ = None *)
(*  let set_loc t _ = t *)(* simple IntID don't hold any location information *) 
(*  let dispatch x _ = x
  let collect _ = [] *)
end  

(* TODO modify NODE_ID so that this is a valid implementation *)
module TcpID = struct
  type loc = Async_extra.Import.Socket.Address.Inet.t Tcp.where_to_connect
  type t = int * loc
  let create id (host,prt) = (id,Tcp.to_host_and_port host prt)
  let get_loc = snd
end

module type ENTRY = sig 
  type t with bin_io,sexp
  val to_string: t -> string
end 

module LogEntry: ENTRY = struct
  (*TODO copy over proper application from other .mls *)
  type t = A | B | C with bin_io,sexp
  let to_string = function
    | A -> "A"
    | B -> "B"
    | C -> "C"
end


module type LOG = functor (E: ENTRY) -> sig
  type t with bin_io,sexp
  val init: unit -> t
  val append: t -> E.t -> t
  val to_string: t -> string
end

module ListLog : LOG =
  functor (LogEntry: ENTRY) ->  struct
  type t = LogEntry.t list with bin_io,sexp
  let init () = []
  let append t x = x::t
  let to_string = List.to_string ~f:LogEntry.to_string
end

module Event = struct 
  type ('a,'b,'c) t = E of ('a * 'b * ('a,'b,'c) event)
                    | N of ('a * 'b * failures)
  and ('a,'b,'c) event = ('c -> ('c * ('a,'b,'c) t list))

  let compare x y = match x,y with
  | (E (xt,_,_),E (yt,_,_)) 
  | (N (xt,_,_),N (yt,_,_)) 
  | (N (xt,_,_),E (yt,_,_)) 
  | (E (xt,_,_),N (yt,_,_)) 
  -> compare xt yt

  
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

