open Core.Std
open Common

module type MACHINE = sig
  type t with sexp (*holds the state of the machine *)
  type cmd with sexp
  type res with sexp
  val commit_many: t -> cmd list -> t
  val init : unit -> t
  val to_string : t -> string
  val cmd_to_string : cmd -> string
  val res_to_string : res -> string
  val gen_workload : int -> cmd list
  val gen_results : int -> res list
  val get_last_res: t -> res
  val check_cmd: t -> cmd -> res option
end

module KeyValStr : MACHINE = struct
 
  type mach_cmd = 
    | Add of int * string 
    | Find of int 
  with bin_io,sexp

  type cmd = int * mach_cmd with bin_io, sexp

  type res = string option with bin_io, sexp

  let res_to_string = function
  | None -> "NotFound"
  | Some x -> ("Found "^x)
  
  let mach_cmd_to_string = function 
    | Add (k,v) -> " ADD: "^(Int.to_string k)^", "^v
    | Find k -> " FIND: "^(Int.to_string k)

  let cmd_to_string (serial_num,cmd) =
    " SERIAL #: "^(Int.to_string serial_num)^": "^(mach_cmd_to_string cmd)

  type t = 
    { mach: (int * string) list;
      last_serial: int;
      last_response: res;
      serial_applied: int list
      } with sexp

  let commit state (id, mach_cmd) = 
    if (id=state.last_serial) then state 
    else if (id=state.last_serial+1) then (
    match mach_cmd with
    | Add (key,value) -> 
      let new_state = List.Assoc.add state.mach key value in
      { mach = new_state; last_serial=id; last_response =(List.Assoc.find new_state key); serial_applied = id::state.serial_applied; }
    | Find key -> 
      { state with last_serial=id; last_response =(List.Assoc.find state.mach key); serial_applied = id::state.serial_applied; } )
    else assert false


  let rec commit_many state cmds = 
  (*assume that commands are sorted so there head is first to be applied *)
    match cmds with
    | x::[] -> commit state x
    | x::xs -> 
      commit_many (commit state x) xs
    | [] -> state

  let init () = 
    { mach = []; last_serial=0;last_response=None; serial_applied=[]; }

  let to_string kvs = 
  "Key Value Store: "^(List.to_string ~f:( fun (k,v) -> (Int.to_string k)^" "^v) kvs.mach)^
  " Last serial: "^(Int.to_string kvs.last_serial)^
  " Last response: "^(res_to_string kvs.last_response)^
  " Serial Applied: "^(List.to_string ~f:Int.to_string kvs.serial_applied )^
  "\n"

  (* let sample_workload  = [
    Add (1,"a"); Add (2,"b"); Add(3,"c")] *)
  let gen_workload (size:int) : cmd list = 
    List.map (List.range ~stop:`inclusive 1 size) ~f:(fun x -> x, Add (x,Int.to_string x)) 

  let gen_results (size:int) : res list = 
    List.map (List.range ~stop:`inclusive 1 size) ~f:(fun x -> Some (Int.to_string x)) 

  let get_last_res s = s.last_response
  let check_cmd s (serial_num,_) = 
    if serial_num = s.last_serial 
    then Some s.last_response
  else None


end
