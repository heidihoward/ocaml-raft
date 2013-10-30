open Core.Std
open Async.Std

module type Machine = sig
  type 'a cmd (*possible commands*)
  val commit : 'a cmd -> 'a 
  val cmd_of_string : string -> 'a cmd 
end

module KeyVal : Machine = struct
  let store = Int.Table.create () ~size:10 
  
  type _ cmd = 
    | Remove: int -> unit cmd
    | Add: (int * string) -> unit cmd
    | Find: int ->  string cmd

  let commit : type a. a cmd -> a   = 
    function
    | Remove key -> Hashtbl.remove store key 
    | Add (key,data) -> ignore(Hashtbl.add store ~key ~data)
    | Find key -> "hello"

  let cmd_of_string (str:string) :_ cmd  = 
    match String.split str ~on:' ' with
    | ["add";key;value] -> Add (int_of_string key, value)
    | ["remove";key] -> Remove (int_of_string key)
    | ["find";key] -> Find (int_of_string key)

end

module DisLock : Machine = struct
  let lock = ref false
  
  type _ cmd = 
    | Lock: unit cmd
    | Unlock: unit cmd

  let commit :  type a. a cmd -> a  =
    function
    | Lock -> lock := false
    | Unlock -> lock := true

  let cmd_of_string =
    function
    | "lock" -> Lock
    | "unlock" -> Unlock

end 
