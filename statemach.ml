open Core.Std


module type MACHINE = sig
  type t (*holds the state of the machine *)
  type cmd
  val commit : t -> cmd -> t 
end

module KeyValStr : MACHINE = struct
  type t = (int,string) list

  type cmd = Add of int * string 

  let commit state = function 
    | Add (key,value) -> List.Assoc.add state key value
end
