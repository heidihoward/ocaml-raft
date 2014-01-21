module type TIME =
  sig
    type t
    type span
    val init : unit -> t
    val compare : t -> t -> int
    val add : t -> span -> t
    val succ : t -> t
    val diff : t -> t -> span
    val span_of_int : int -> span
    val span_of_float : float -> span
    val span_to_string : span -> string
    val to_string : t -> string
    val wait_until : t -> unit
    val store : t -> unit -> t 
  end
module FakeTime : TIME
module RealTime : TIME
