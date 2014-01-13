open Core.Std
open Common

(*module type PACKET = sig
  type t 
end *)

module RequestVoteArg  = struct
  type t = { term: Index.t;
             cand_id: IntID.t;
             last_index: Index.t;
             last_term: Index.t; }
end

module RequestVoteRes = struct
  type t = { term: Index.t;
             votegranted: bool}
end

module HeartbeatArg = struct
  type t = { term: Index.t;
             lead_id: IntID.t;}
end 

module HeartbeatRes = struct
  type t = { term: Index.t; }
end

type t =  RequestVoteArg of RequestVoteArg.t 
          | RequestVoteRes of RequestVoteRes.t 
          | HeartbeatArg of HeartbeatArg.t 
          | HeartbeatRes of HeartbeatRes.t 
