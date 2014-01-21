module type MACHINE =
  sig
    type t
    type cmd
    val commit : t -> cmd -> t
    val init : unit -> t
    val to_string : t -> string
    val cmd_to_string : cmd -> string
    val t_of_sexp : Sexplib.Type.t -> t
    val sexp_of_t : t -> Sexplib.Type.t
    val cmd_of_sexp : Sexplib.Type.t -> cmd
    val sexp_of_cmd : cmd -> Sexplib.Type.t
  end
module KeyValStr : MACHINE
