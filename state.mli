open Core.Std
open Async.Std

type t

val init : unit -> t
val set_id : t -> int -> unit
val get_id : t -> int
val add_nodes: t -> (int * (string * int)) list -> unit
val get_addr_all: t -> (int * (string * int)) list
val get_addr_by_id: t -> int -> (string * int) option
val election_timeout: t -> unit
val election_status: t -> Role.election_outcome
val get_term: t -> int
val get_lastlogindex: t -> int
val get_lastlogterm: t -> int


