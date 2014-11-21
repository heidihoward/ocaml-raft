open Core_kernel.Std
open Common

type t = int
type span = int
let init = 0
let compare a b = compare a b
let add a b = a + b
let diff a b = a - b
(*  let t_of_int t = t *)
let span_of_int s = s
let span_of_float = Float.to_int
let span_to_string = string_of_int
let to_string t = string_of_int t
let to_int t = t


