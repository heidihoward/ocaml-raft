open Core.Std
open Async.Std

type t

val empty : t
val set_id : t -> int -> unit
val get_id : t -> int
val add_nodes: t -> (int * (string * int)) list -> unit
val get_addr_all: t -> (int * (string * int)) list
