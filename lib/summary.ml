open Core.Std
open Common

type t =
  {
  reason: termination;
  time: string;
  replica_pkts: int;
  client_pkts: int;
  leader_est: string;
  client_latency: string;
  avalability: float;
  election_time: string; 
  }

let to_string x = 
  "Reason: "^termination_to_string x.reason^
  "\nTime: "^x.time^
  "\nReplica Packets: "^(Int.to_string x.replica_pkts)^
  "\nClient Packets: "^(Int.to_string x.client_pkts)^
  "\nLeader Established: "^x.leader_est^
  "\nClient Latency: "^x.client_latency^
  "\nAvalability: "^(Float.to_string x.avalability)^
  "\nElectionTime: "^x.election_time^
  "\n"