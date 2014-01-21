module RequestVoteArg :
  sig
    type t = {
      term : Common.Index.t;
      cand_id : Common.IntID.t;
      last_index : Common.Index.t;
      last_term : Common.Index.t;
    }
    val __t_of_sexp__ : Sexplib.Type.t -> t
    val t_of_sexp : Sexplib.Type.t -> t
    val sexp_of_t : t -> Sexplib.Type.t
    val to_string : t -> string
  end
module RequestVoteRes :
  sig
    type t = { term : Common.Index.t; votegranted : bool; }
    val __t_of_sexp__ : Sexplib.Type.t -> t
    val t_of_sexp : Sexplib.Type.t -> t
    val sexp_of_t : t -> Sexplib.Type.t
    val to_string : t -> string
  end
module HeartbeatArg :
  sig
    type t = { term : Common.Index.t; lead_id : Common.IntID.t; }
    val __t_of_sexp__ : Sexplib.Type.t -> t
    val t_of_sexp : Sexplib.Type.t -> t
    val sexp_of_t : t -> Sexplib.Type.t
    val to_string : t -> string
  end
module HeartbeatRes :
  sig
    type t = { term : Common.Index.t; }
    val __t_of_sexp__ : Sexplib.Type.t -> t
    val t_of_sexp : Sexplib.Type.t -> t
    val sexp_of_t : t -> Sexplib.Type.t
    val to_string : t -> string
  end
module ClientArg : sig type t = { cmd : string; } end
module ClientRes :
  sig type t = { success : bool; leader : Common.IntID.t option; } end

(*type t =
    RequestVoteArg of RequestVoteArg.t
  | RequestVoteRes of RequestVoteRes.t
  | HeartbeatArg of HeartbeatArg.t
  | HeartbeatRes of HeartbeatRes.t
  | ClientArg of ClientArg.t
  | ClientRes of ClientRes.t *)
