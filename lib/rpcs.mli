open Core.Std
open Common

type cmd

module RequestVoteArg :
  sig
    type t = {
      term : Index.t;
      cand_id : IntID.t;
      last_index : Index.t;
      last_term : Index.t;
    } with sexp
     val to_string : t -> string   
  end

module RequestVoteRes :
  sig
    type t = { term : Index.t; votegranted : bool; 
                 replyto: RequestVoteArg.t} with sexp
    val to_string : t -> string
  end

module AppendEntriesArg :
  sig
    type t = { 
               term : Index.t; 
               lead_id : IntID.t; 
              prevLogIndex: Index.t;
             prevLogTerm: Index.t; 
             leaderCommit: Index.t;
             entries: (Index.t * Index.t * Sexp.t) list; } with sexp
    val to_string : t -> string
  end

module AppendEntriesRes :
  sig
    type t = { success: bool; term : Index.t; 
                 follower_id: IntID.t;
                 replyto: AppendEntriesArg.t } with sexp
    val to_string : t -> string
  end

module ClientArg : 
sig type t = { cmd : Sexp.t;} with sexp 
val to_string : t -> string
end

module ClientRes :
  sig type t = { success : Sexp.t option; node_id: IntID.t; leader : IntID.t option;
               replyto: ClientArg.t } with sexp
  val to_string : t -> string
   end

(*type t =
    RequestVoteArg of RequestVoteArg.t
  | RequestVoteRes of RequestVoteRes.t
  | AppendEntriesArg of AppendEntriesArg.t
  | AppendEntriesRes of AppendEntriesRes.t
  | ClientArg of ClientArg.t
  | ClientRes of ClientRes.t *)
