open Core.Std
(*open Async.Std*)
(* This module contains basic functions and modules used through, with no
 * external dependances on any other modules *)


type role = Follower | Candidate | Leader with sexp
type 'a status = Live of 'a | Down of 'a | Notfound 
type failures = Wake | Kill
type termination = LeaderEst | WorkloadEmpty | Timeout

type datacollection = {mutable pkts: int; mutable client_pkts: int; mutable firstelc: int option}

let termination_to_string = function
  | LeaderEst -> "LeaderEstablished"
  | WorkloadEmpty -> "WorkloadEmpty"
  | Timeout -> "Timeout"

let string_of_role = function
  | Follower -> "Follower"
  | Candidate -> "Candidate"
  | Leader -> "Leader"

let string_of_option f = function 
  | None -> "none" 
  | Some x -> f x

module NumberGen = struct
  (* TODO: these are dealing with discite values but i think i need a seperate
   * ones for continous *)

  let () = Random.self_init ()
  let fixed x () =  x

  let uniform  min max () = (Random.float (max-.min) +. min)

  let exp lam const () = 
    (* TODO fix this *)
    const +. (-1.0 /. lam)*.log(Random.float 1.0)

 let string_to_dist str =
   let flt = Float.of_string in
   match (String.split str ~on:'-') with
   | "Fixed"::value::_ -> fixed (flt value)
   | "Uniform"::min::max::[] -> uniform (flt min) (flt max)
   | "Exp"::lamda::const::[] -> exp (flt lamda) (flt const)
   | er ->  eprintf "failure to parse: %s" (List.to_string ~f:(fun x -> x) er) ; exit 1

end

module type PARAMETERS = sig
  val timeout: unit -> role -> float
  val nodes: int
  val pkt_delay: unit -> float
  val debug_mode: bool
  val nxt_failure: (unit -> float) option
  val nxt_recover: (unit -> float) option
  val term_conditions : termination -> bool
  val workload_size: int
  val term_time : int
    val client_wait : int
  val client_timeout : int
end

module Index = struct
  type t = int with compare,sexp,bin_io
  let succ = succ
  let pred = pred
  let init () = 0
  (*TODO: consider is elections can start fast with randon inital value *)
  let to_string = string_of_int
end

module IntID  = struct
  type t = int with sexp,bin_io
  let from_int x = x
  let to_int x  = x
  let equal = Int.equal
  let to_string = string_of_int
end  



(* This is no used but its too beautiful to delete 
module ListLog =
  functor (Entry: ENTRY) -> ( struct
  type entry = Entry.t
  type t = Entry.t list with bin_io,sexp
  let init () = []
  let append t x = x::t
  let to_string = List.to_string ~f:Entry.to_string
end : LOG)
*)

module Event = struct 
  (* TODO: make this much better and actually inforce use of state calls *)
    type ('time, 'id, 'state,'client) t =
      | RaftEvent of ('time * 'id * ('time, 'id, 'state,'client) event)
      | SimulationEvent of ('time * 'id * failures)
      | ClientEvent of ('time *  ('time, 'id, 'state,'client) client)
      | Terminate of 'time
    and ('time, 'id, 'state,'client) event = 'state -> 'state * ('time, 'id, 'state,'client) t list
    and ('time, 'id, 'state,'client) client = 'client -> 'client * ('time, 'id, 'state,'client) t list

  let get_time = function
  | RaftEvent (x,_,_)
  | SimulationEvent (x,_,_)
  | ClientEvent (x,_) -> x
  | Terminate x -> x

  let compare x y = compare (get_time x) (get_time y)

  
end 





