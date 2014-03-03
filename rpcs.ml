open Core.Std
open Common

(** This module specifics the arguments and results of RPC's *)

(*module packet = 
 functor (Pkt: sig type t with sexp) ->  struct
  
  let t_to_string (t:Pkt.t) = 
  sexp_of_t t |> Sexp.to_string

end 
*)

module RequestVoteArg  = struct
  type t = { term: Index.t;
             cand_id: IntID.t;
             last_index: Index.t;
             last_term: Index.t; } with sexp

  let to_string t = 
    "--> RequestVote Request "^(sexp_of_t t |> Sexp.to_string)
end

module RequestVoteRes = struct
  type t = { term: Index.t;
             votegranted: bool} with sexp
  let to_string t =
    "--> RequestVote Response "^(sexp_of_t t |> Sexp.to_string)
end

module AppendEntriesArg = struct
  type t = { term: Index.t;
             lead_id: IntID.t;
             entries: (Index.t * Index.t * Sexp.t) list} with sexp
  let to_string t =
    "--> AppendEntries Request "^(sexp_of_t t |> Sexp.to_string)
end 

module AppendEntriesRes = struct
  type t = { term: Index.t; } with sexp
  let to_string t = 
    "--> AppendEntries Response "^(sexp_of_t t |> Sexp.to_string)
end

module ClientArg = struct
  type t = { cmd: Sexp.t } with sexp
  let to_string t = 
    "--> Client Request "^(sexp_of_t t |> Sexp.to_string)   
end

module ClientRes = struct
  type t = { success: bool;
             node_id: IntID.t;
             leader:  IntID.t option; } with sexp
  let to_string t = 
    "--> Client Response "^(sexp_of_t t |> Sexp.to_string)   
end

type t =  RequestVoteArg of RequestVoteArg.t 
          | RequestVoteRes of RequestVoteRes.t 
          | AppendEntriesArg of AppendEntriesArg.t 
          | AppendEntriesRes of AppendEntriesRes.t
          | ClientArg of ClientArg.t
          | ClientRes of ClientRes.t
