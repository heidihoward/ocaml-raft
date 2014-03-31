exception SPLMonitorFailure of string

type s = RaftMonitor.s

type t
val init : unit -> t
val tick : t -> s -> t
