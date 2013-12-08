open Core.Std
open Async.Std

type election_outcome = 
   In_progress 
 | Timeout
 | Step_up
 | Step_down

type t = 
   Follower of Time.Ofday.t 
 | Candidate of election_outcome 
 | Leader

let to_string = function
  | Follower _ -> "Follower"
  | Candidate _ -> "Candidate"
  | Leader -> "Leader"

