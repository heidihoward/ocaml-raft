exception Bad_statecall

type s = RaftMonitor.s

type t
val init : unit -> t
val tick : t -> s -> t
