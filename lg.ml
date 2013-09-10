open Core.Std
open Async.Std

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
