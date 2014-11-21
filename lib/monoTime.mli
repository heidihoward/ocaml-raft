(** [TIME] is an abstract notion, used here for sequencing events *) 
type t
type span
val init :  t
val compare : t -> t -> int (*TODO ask anil why explicity write this instead
of using Core's with compare *)
val add : t -> span -> t
val diff : t -> t -> span
val span_of_int : int -> span
val span_to_string: span -> string
val to_string: t -> string
val to_int: t -> int