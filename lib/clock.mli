(** [TIME] is an abstract notion, used here for sequencing events *) 

module type TIME =
  sig
    type t
    type span
    val init : unit -> t
    val compare : t -> t -> int
    val add : t -> span -> t
    val diff : t -> t -> span
    val span_of_int : int -> span
    val span_of_float : float -> span
    val span_to_string : span -> string
    val to_string : t -> string
    val to_int: t -> int
    val wait_until : t -> unit
    val store : t -> unit -> t 
  end
module FakeTime : TIME
module RealTime : TIME
