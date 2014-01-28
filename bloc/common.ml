open Core.Std

module type STATE = sig
  type t 
  val init: unit -> t
end

module State : STATE = struct
  type t = string
  let init () = "hello"
end

module type NODE = sig
  val state
  val startup: unit 
end

module Protocol = struct
  let state = State.init () 

  type t = IncomingHello of string 
         | OutGoingHello of string
         | Startup
         | StupDown

  let process msg = match with
    | 

  
