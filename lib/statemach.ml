open Core.Std
open Common

module type MACHINE = sig
  type t with sexp (*holds the state of the machine *)
  type cmd with sexp
  type res with sexp
  val commit_many: t -> cmd list -> (t * res)
  val init : unit -> t
  val to_string : t -> string
  val cmd_to_string : cmd -> string
  val gen_workload : int -> (int * cmd) list
end

module KeyValStr : MACHINE = struct
  
  type cmd = 
    | Add of int * string 
    | Find of int 
  with bin_io,sexp

  type res = string option with bin_io, sexp
  
  let cmd_to_string = function 
    | Add (k,v) -> "ADD: "^(Int.to_string k)^", "^v
    | Find k -> "FIND: "^(Int.to_string k)

  type t = (int * string) list with sexp

  let commit state cmd = 
    match cmd with
    | Add (key,value) -> 
      let new_state = List.Assoc.add state key value in
      (new_state, List.Assoc.find new_state key)
    | Find key -> 
      (state, List.Assoc.find state key)


  let rec commit_many state cmds = 
  (*assume that commands are sorted so there head is first to be applied *)
    match cmds with
    | x::[] -> commit state x
    | x::xs -> 
      let new_state,_ = (commit state x) in
      commit_many new_state xs
    | [] -> assert false

  let init () = []

  let to_string = List.to_string ~f:( fun (k,v) -> (Int.to_string k)^v) 

  (* let sample_workload  = [
    Add (1,"a"); Add (2,"b"); Add(3,"c")] *)
  let gen_workload (size:int) : (int * cmd) list = 
    List.init ~f:(fun x -> x, Add (0,Int.to_string x)) size

end
