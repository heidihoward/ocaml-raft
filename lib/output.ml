open Yojson.Basic
open Core_kernel.Std

let debug_mode = false
let json_mode = false

let json_breaker st sp str = 
  (if not(st) then String.set str 0 ' ' else ());
  if not(sp) then String.set str ((String.length str)-1) ',' else ();
  str

let debug x = if (debug_mode) then (printf " %s  \n" x) else ()
let json ?start:(st=true) ?stop:(sp=true) x = 
  if (json_mode) then (printf " %s \n" (json_breaker st sp (to_string x))) else ()