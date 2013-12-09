open Core.Std
(*
module type SPAN = sig
  type t
  val of_seconds : int -> t
end
*)
module type TIME = sig
  type t
  type span
  val init : unit -> t
  val compare : t -> t -> int (*TODO ask anil why explicity write this instead
  of using Core's with compare *)
  val add : t -> span -> t
  val succ: t -> t
  val diff : t -> t -> span
  val t_of_int : int -> t
  val span_of_int : int -> span
  val to_string: t -> string
  val wait : t -> span -> t
end 

module FakeTime : TIME = struct
  type t = int
  type span = int
  let init () = 0
  let compare a b = compare a b
  let add a b = a + b
  let diff a b = a - b
  let t_of_int t = t
  let span_of_int s = s
  let succ t = t+1
  let to_string t = string_of_int t
  let wait t span = 
    Printf.printf "fake wait %s\n%!" (to_string t); 
    add t span
end

module RealTime : TIME = struct
  type t = Time.t
  type span = Time.Span.t
  let init () = Time.now ()
  let compare = Time.compare
  let add a b = Time.add a b
  let t_of_int _t = Time.now () (* TODO fixme *)
  let span_of_int s = Time.Span.create ~sec:s ()
  let succ a = add a (span_of_int 1)
  let to_string t = Time.to_string t
  let diff a b = Time.diff a b

  let wait t span =
    Printf.printf "real wait %s\n" (to_string t);
    let sec = Time.Span.to_sec span |> Float.to_int in
    Unix.sleep sec;
    Time.now ()
end

module Counter(T:TIME) = struct
  let run () =
    let t = T.init () in
    let initial_span = T.diff t t in
    let last_time = T.wait t initial_span in
    print_endline (T.to_string last_time)
end

module FakeCounter = Counter(FakeTime)
module RealCounter = Counter(RealTime)
(*
let () =
  FakeCounter.run ();
  RealCounter.run () *)
