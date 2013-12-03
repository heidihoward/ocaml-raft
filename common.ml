open Core.Std

module type INDEX = sig
  type t
  val succ: t -> t
  val init: unit -> t
end 

module Index : INDEX = struct
  type t = int with compare
  let succ = succ
  let init () = 0
end

module type ID = sig
  type t
  val from_int: int -> t
  val to_int: t -> t
  val comp: t -> t -> bool
  val print: t -> string
end 

module IntID : ID  = struct
  type t = int
  let from_int x = x
  let to_int x  = x
  let comp t1 t2 = phys_equal t1 t2
  let print x = string_of_int x
end

module type ENTRY = sig type t end 

module LogEntry: ENTRY = struct
  (*TODO copy over proper application from other .mls *)
  type t = A | B | C
end


module type LOG = functor (E: ENTRY) -> sig
  type t
  val init: unit -> t
  val append: t -> E.t -> t
end

module ListLog : LOG =
  functor (LogEntry: ENTRY) ->  struct
  type t = LogEntry.t list
  let init () = []
  let append t x = x::t
end


