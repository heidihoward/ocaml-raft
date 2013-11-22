open Core.Std

module type INDEX = sig
  type t
  val succ: t -> t
  val init: unit -> t
end

module Index : INDEX = struct
  type t = int with compare
  let succ = succ
  let init () = 0
end 

module type LOG =  functor (I: INDEX) -> functor (M:MACHINE) -> sig
  type entry
  type t
  val init: unit -> t
  val append: I.t -> M.cmd -> t -> t
end

module ListLog(I:Index)(M:Machine) : LOG  = struct

type entry = 
 { term: int;
   index: int;
   command: string; }
 with bin_io

type t = entry list with bin_io 

let start :t= []

let append term command log = 
  let index = match List.hd log with 
  |Some entry -> entry.index
  |None -> 0 in
  {term; index; command}::log
  
let getlastindex log = 
  match List.hd log with
  | Some entry -> entry.index
  | None -> 0

let getlastterm log =
  match List.hd log with
  | Some entry -> entry.term
  | None -> 0

end
