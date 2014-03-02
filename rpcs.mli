open Core.Std
open Common

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
    type t = { term : Index.t; votegranted : bool; } with sexp
    val to_string : t -> string
  end

module HeartbeatArg :
  sig
    type t = { term : Index.t; lead_id : IntID.t; } with sexp
    val to_string : t -> string
  end

module HeartbeatRes :
  sig
    type t = { term : Index.t; } with sexp
    val to_string : t -> string
  end

module ClientArg : 
sig type t = { cmd : Sexp.t; } with sexp 
val to_string : t -> string
end

module ClientRes :
  sig type t = { success : bool; node_id: IntID.t; leader : IntID.t option; } with sexp
  val to_string : t -> string
   end

(*type t =
    RequestVoteArg of RequestVoteArg.t
  | RequestVoteRes of RequestVoteRes.t
  | HeartbeatArg of HeartbeatArg.t
  | HeartbeatRes of HeartbeatRes.t
  | ClientArg of ClientArg.t
  | ClientRes of ClientRes.t *)
