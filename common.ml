open Core.Std

let debug_active = ref true

let debug x = if !debug_active then
    (printf " %s \n"  x)


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
  val from_int: int -> t
  val to_int: t -> t
  val comp: t -> t -> bool
  val to_string: t -> string
end 

module IntID : ID  = struct
  type t = int
  let from_int x = x
  let to_int x  = x
  let comp t1 t2 = phys_equal t1 t2
  let to_string = string_of_int
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


