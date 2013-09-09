open Core.Std
open Async.Std

type electon_outcome = 
   None
 | Timeout
 | Step_up
 | Step_down

type t = 
   Follower 
 | Candidate of electon_outcome 
 | Leader

let to_string = function
  | Follower -> "Follower"
  | Candidate _ -> "Candidate"
  | Leader -> "Leader"

let start_up = Follower

let electon_timeout t = match t with
  | Candidate None -> Candidate Timeout
  | x -> x
