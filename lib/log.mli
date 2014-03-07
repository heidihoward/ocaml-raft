open Core.Std
open Common

  (*highest element is always at head *)
  type 'a t with sexp
  val init: unit -> 'a t
  val append: (Index.t * Index.t * 'a) ->'a t-> 'a t
  val appends: (Index.t * Index.t * 'a) list ->'a t -> 'a t
  val last_index_term:'a t -> (Index.t * Index.t)
  val specific_index_term: Index.t -> 'a t -> (Index.t * Index.t)
  val consistency_check: 'a t-> Index.t -> Index.t -> [`Consistent | `Inconsistent]
  val to_string: cmd_to_string:('a -> string) -> 'a t -> string
  val to_commit: Index.t -> Index.t -> 'a t-> 'a list
  val cut_entries: Index.t -> 'a t ->'a t
  val get_entries: Index.t -> 'a t -> (Index.t * Index.t * 'a) list