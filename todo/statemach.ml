open Core.Std
open Async.Std

module type MACHINE = sig
  (* TODO reimplement the GADT's here *)
  type cmd (*possible commands*)
  type outcome
  val commit : cmd -> outcome
  val cmd_of_string : string -> cmd 
end

module KeyVal : MACHINE = struct
  let store = Int.Table.create () ~size:10 
  
  type cmd = 
    | Remove of int 
    | Add of (int * string)
    | Find of int 

  type outcome = 
    | None
    | NotFound
    | Found of string

  let commit  = 
    function
      | Remove key -> Hashtbl.remove store key; None 
    | Add (key,data) -> ignore(Hashtbl.add store ~key ~data); None
    | Find key -> Found "hello" (* TODO *)

  let cmd_of_string (str:string)  = 
    match String.split str ~on:' ' with
    | ["add";key;value] -> Add (int_of_string key, value)
    | ["remove";key] -> Remove (int_of_string key)
    | ["find";key] -> Find (int_of_string key)

end

module DisLock : MACHINE = struct
  let lock = ref false
  
  type cmd = 
    | Lock
    | Unlock

  type outcome = 
    | Success
    | Failure

  let commit  =
    function
      | Lock -> lock := false; Success
    | Unlock -> lock := true; Success

  let cmd_of_string =
    function
    | "lock" -> Lock
    | "unlock" -> Unlock

end 
