open Core.Std
(*open Async.Std*)
(* This module contains basic functions and modules used through, with no
 * external dependances on any other modules *)


type role = Follower | Candidate | Leader with sexp
type 'a status = Live of 'a | Down of 'a | Notfound 
type failures = Wake | Kill

let string_of_role = function
  | Follower -> "Follower"
  | Candidate -> "Candidate"
  | Leader -> "Leader"


module NumberGen = struct
  (* TODO: these are dealing with discite values but i think i need a seperate
   * ones for continous *)

  let () = Random.self_init ()
  let fixed x () =  x

  let uniform  min max () = (Random.float (max-.min) +. min)

  let exp lam () = (-1.0 /. lam)*.log(Random.float Float.max_finite_value)

 let string_to_dist str =
   let flt = Float.of_string in
   printf " %s \n" str;
   match (String.split str ~on:'-') with
   | "Fixed"::value::_ -> fixed (flt value)
   | "Uniform"::min::max::[] -> uniform (flt min) (flt max)
   | "Exp"::lamda::[] -> exp (flt lamda)
   | er ->  eprintf "failure to parse: %s" (List.to_string ~f:(fun x -> x) er) ; exit 1

end

module type PARAMETERS = sig
  val timeout: role -> float
  val nodes: int
  val pkt_delay: unit -> float
  val termination: int
  val debug_mode: bool
  val nxt_failure: (unit -> float) option
  val nxt_recover: (unit -> float) option
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
(*
(* TODO modify NODE_ID so that this is a valid implementation *)
module TcpID = struct
  type loc = Async_extra.Import.Socket.Address.Inet.t Tcp.where_to_connect
  type t = int * loc
  let create id (host,prt) = (id,Tcp.to_host_and_port host prt)
  let get_loc = snd
end
*)
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


module type LOG = sig
  type t with bin_io,sexp
  type entry
  val init: unit -> t
  val append: t -> entry -> t
  val to_string: t -> string
end

module ListLog =
  functor (Entry: ENTRY) -> ( struct
  type entry = Entry.t
  type t = Entry.t list with bin_io,sexp
  let init () = []
  let append t x = x::t
  let to_string = List.to_string ~f:Entry.to_string
end : LOG)

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

