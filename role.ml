open Core.Std
open Async.Std

type t = Follower | Candidate | Leader

let to_string = function
  | Follower -> "Follower"
  | Candidate -> "Candidate"
  | Leader -> "Leader"

let start_up = Follower


