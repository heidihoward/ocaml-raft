open Core.Std
open Common

module type MACHINE = sig
  module Entry : ENTRY
  type t (*holds the state of the machine *)
  type cmd
  val commit : t -> cmd -> t
  val init : unit -> t
end

module KeyValStr : MACHINE = struct
  
  type cmd = Add of int * string with bin_io,sexp
  
  module Entry : ENTRY = struct
    type t = cmd with bin_io,sexp
    let to_string _ = "heelo"
  end

  type t = (int * string) list

  let commit state cmd = 
    match cmd with
    | Add (key,value) -> List.Assoc.add state key value

  let init () = []
end
