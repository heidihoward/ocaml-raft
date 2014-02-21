open Core.Std
open Common

module type SecretDataStructure =
  sig
  type item
  type t
  val init : unit -> t
  val add : item -> t -> t
  val get_min : t -> item option
end

module SortedList (E: sig type t with compare end) = 
  struct
  type item = E.t
  type t = item list
  let init () = []
  let add x lst = List.merge [x] lst ~cmp:E.compare
  let get_min = function
     | [] -> None
     | x::_ -> Some x
end

module MystringList = (SortedList(String) : SecretDataStructure with type item = String.t)
module MystringListlist = (SortedList(String) : SecretDataStructure with type item = String.t list)

let () =
   let s1 = "hello" in
   let s2 = "bye bye" in
   printf "S1: %s S2: %s" s1 s2;
   MystringList.init ()
   |> MystringList.add s1 
   |> MystringList.add s2
   |> MystringList.get_min
   |> printf "minimum: %s" 
