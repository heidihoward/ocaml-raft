module type MACHINE =
  sig
  type t with sexp (*holds the state of the machine *)
  type cmd with sexp
  type res with sexp
  val commit_many: t -> cmd list -> (t * res)
  val init : unit -> t
  val to_string : t -> string
  val cmd_to_string : cmd -> string
  val gen_workload : int -> (int * cmd) list
  end
module KeyValStr : MACHINE
