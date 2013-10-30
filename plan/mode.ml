open Core.Std
open Async.Std

module type Mode = sig
  val run : State.t -> (Common.modes * State.t) Deffered.t
end

module Leader : Mode = struct 
  let run state = (Follower,state)
end

module Follower : Mode = struct 
  let run state = (Follower,state)
end

module Candidate : Mode = struct
  let run state = (Follower,state)
end

