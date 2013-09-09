open Core.Std
open Async.Std

type entry = 
 { term: int;
   index: int;
   command: string; }

type t = entry list 

let start :t= []

let append term command log = 
  let index = match List.hd log with 
  |Some entry -> entry.index
  |None -> 0 in
  {term; index; command}::log
  

