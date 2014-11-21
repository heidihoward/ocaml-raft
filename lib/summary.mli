open Core_kernel.Std
open Common

type t =
  {
  reason: termination;
  time: string;
  replica_pkts: int;
  client_pkts: int;
  leader_est: int option;
  client_latency: string;
  avalability: float;
  election_time: string; 
  }

val to_string: t -> string
