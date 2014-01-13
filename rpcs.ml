open Core.Std
open Common

module type PACKET = sig
  type t 
end

module RequestVoteArg :PACKET = struct
  type t = { term: Index.t;
             cand_id: IntID.t;
             last_index: Index.t;
             last_term: Index.t; }
end

module RequestVoteRes :PACKET = struct
  type t = { term: int;
             votegranted: bool}
end

module HeartbeatArg :PACKET = struct
  type t = { term: Index.t;
             lead_id: IntID.t;}
end 

module HeartbeatRes :PACKET = struct
  type t = { term: int;
             success: bool }
end

type pkt =  RequestVoteArg of RequestVoteArg.t 
          | RequestVoteRes of RequestVoteRes.t 
          | HeartbeatArg of HeartbeatArg.t 
          | HeartbeatRes of HeartbeatRes.t 
