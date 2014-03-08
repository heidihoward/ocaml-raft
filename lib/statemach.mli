module type MACHINE =
  sig
    type t with sexp
    type cmd with sexp
    val commit : t -> cmd -> t
    val commit_many: t -> cmd list -> t
    val init : unit -> t
    val to_string : t -> string
    val cmd_to_string : cmd -> string
    val gen_workload : int -> cmd list
  end
module KeyValStr : MACHINE
