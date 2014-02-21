open Core.Std
open Common

module type MACHINE = sig
  type t with sexp (*holds the state of the machine *)
  type cmd with sexp
  val commit : t -> cmd -> t
  val init : unit -> t
  val to_string : t -> string
  val cmd_to_string : cmd -> string
  val sample_workload : cmd list
end

module KeyValStr : MACHINE = struct
  
  type cmd = Add of int * string with bin_io,sexp
  
  let cmd_to_string = function 
    | Add (k,v) -> (Int.to_string k)^v

  type t = (int * string) list with sexp

  let commit state cmd = 
    match cmd with
    | Add (key,value) -> List.Assoc.add state key value

  let init () = []

  let to_string = List.to_string ~f:( fun (k,v) -> (Int.to_string k)^v) 

  let sample_workload  = [
    Add (1,"a"); Add (2,"b"); Add(3,"c")]

end
