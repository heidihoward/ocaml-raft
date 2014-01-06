(** [Cmdstart] handles the command line parsing for the Raft simulator *)

(** [run_disctime] parse cmd line options for a DES *)
val run_disctime :
  nodes:int ->
  term:int ->
  time_min:int ->
  time_max:int ->
  delay_min:int -> delay_max:int -> debug:bool -> iter:int -> data:'a -> unit

  (** [run_realtime] parse cmd line options for a realtime simulation *)
val run_realtime :
  nodes:int ->
  term:int ->
  time_min:int ->
  time_max:int ->
  delay_min:int -> delay_max:int -> debug:bool -> iter:int -> data:'a -> unit
val command : Core.Command.t
