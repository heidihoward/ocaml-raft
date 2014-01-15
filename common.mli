(** Common is the basic module at the foundation of OCaml-Raft, it uses no other
 * ocaml-raft modules and contains much of the basic functionality used throught
 * *)

(* [role] is the basic mode of operation of a node *)
type role = Follower | Candidate | Leader
val __role_of_sexp__ : Sexplib.Type.t -> role
val role_of_sexp : Sexplib.Type.t -> role
val sexp_of_role : role -> Sexplib.Type.t

(** [status] wraps around State.t to simulate node failures *)
type 'a status = Live of 'a | Down of 'a | Notfound

(** [failures] hows the holds the two possible N actions *)
type failures = Wake | Kill

val string_of_role : role -> string
val string_of_option : ('a -> string) -> 'a option -> string

(** [NumberGen] holds the functionality used for generating random numbers for
 * the simulators, all parameters and results are float, even through there use
 * will often be discrite *)
module NumberGen :
  sig
   (** the following take parameters and return random number generaters *)

    val fixed : float -> (unit -> float)
    val uniform : float -> float -> (unit -> float)
    val exp : float -> (unit -> float)
    val string_to_dist : string -> (unit -> float)
  end

(** [PARAMETERS] defines the outcome the command line options are used the
 * simulator *)  
module type PARAMETERS =
  sig
    (* [timeout role] returns a float returning the timeout using the
     * distribution specified eariler *)
    val timeout : role -> float
    val nodes : int
    val pkt_delay : unit -> float
    val termination : int
    val debug_mode : bool
    val nxt_failure : (unit -> float) option
    val nxt_recover : (unit -> float) option
  end

(** [Index] is a single monotonically increasing discrete value *) 
module Index :
  sig
    type t
    val succ : t -> t
    val init : unit -> t
    val to_string : t -> string

    val bin_t : t Bin_prot.Type_class.t0
    val bin_read_t : t Core.Std.Bin_prot.Read.reader
    val __bin_read_t__ : (int -> t) Core.Std.Bin_prot.Read.reader
    val bin_reader_t : t Bin_prot.Type_class.reader0
    val bin_size_t : t Core.Std.Bin_prot.Size.sizer
    val bin_write_t : t Core.Std.Bin_prot.Write.writer
    val bin_writer_t : t Bin_prot.Type_class.writer0
    val t_of_sexp : Sexplib.Type.t -> t
    val sexp_of_t : t -> Sexplib.Type.t
  end

(** [IntID] is the identifier used for nodes, this could later be used to store
 * location information *)
module IntID : 
  sig
    type t
    val from_int : int -> t
    val to_int : t -> int
    val equal : t -> t -> bool
    val to_string : t -> string

    val bin_t : t Bin_prot.Type_class.t0
    val bin_read_t : t Core.Std.Bin_prot.Read.reader
    val __bin_read_t__ : (int -> t) Core.Std.Bin_prot.Read.reader
    val bin_reader_t : t Bin_prot.Type_class.reader0
    val bin_size_t : t Core.Std.Bin_prot.Size.sizer
    val bin_write_t : t Core.Std.Bin_prot.Write.writer
    val bin_writer_t : t Bin_prot.Type_class.writer0
    val t_of_sexp : Sexplib.Type.t -> t
    val sexp_of_t : t -> Sexplib.Type.t
  end

(** [LOG] is a cut down version of list used for the replication log to ensure
   * append only *)
module type LOG =
  sig
    type 'a t
    val init : unit -> 'a t
    val cons : 'a -> 'a t -> 'a t
    val to_string : f:('a -> string) -> 'a t -> string

    val t_of_sexp : (Sexplib.Type.t -> 'a) -> Sexplib.Type.t -> 'a t
    val sexp_of_t : ('a -> Sexplib.Type.t) -> 'a t -> Sexplib.Type.t
    val bin_t : 'a Bin_prot.Type_class.t0 -> 'a t Bin_prot.Type_class.t0
    val bin_read_t :
      'a Core.Std.Bin_prot.Read.reader -> 'a t Core.Std.Bin_prot.Read.reader
    val __bin_read_t__ :
      'a Core.Std.Bin_prot.Read.reader ->
      (int -> 'a t) Core.Std.Bin_prot.Read.reader
    val bin_reader_t :
      'a Bin_prot.Type_class.reader0 -> 'a t Bin_prot.Type_class.reader0
    val bin_size_t :
      'a Core.Std.Bin_prot.Size.sizer -> 'a t Core.Std.Bin_prot.Size.sizer
    val bin_write_t :
      'a Core.Std.Bin_prot.Write.writer ->
      'a t Core.Std.Bin_prot.Write.writer
    val bin_writer_t :
      'a Bin_prot.Type_class.writer0 -> 'a t Bin_prot.Type_class.writer0
  end
module ListLog : LOG

module Event :
  sig
    type ('a, 'b, 'c) t =
        E of ('a * 'b * ('a, 'b, 'c) event)
      | N of ('a * 'b * failures)
    and ('a, 'b, 'c) event = 'c -> 'c * ('a, 'b, 'c) t list
    val compare : ('a, 'b, 'c) t -> ('a, 'd, 'e) t -> int
  end

module type EVENTLIST =
  sig
    type ('a, 'b, 'c) t
    val from_list : ('a, 'b, 'c) Event.t list -> ('a, 'b, 'c) t
    val to_list : ('a, 'b, 'c) t -> ('a, 'b, 'c) Event.t list
    val hd : ('a, 'b, 'c) t -> ('a, 'b, 'c) Event.t option
    val add : ('a, 'b, 'c) Event.t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  end

module EventList :
  sig
    type ('a, 'b, 'c) t = ('a, 'b, 'c) Event.t list
    val from_list : ('a, 'b, 'c) Event.t list -> ('a, 'b, 'c) Event.t list
    val to_list : 'a -> 'a
    val hd : 'a list -> ('a * 'a list) option
    val add :
      ('a, 'b, 'c) Event.t list ->
      ('a, 'b, 'c) Event.t list -> ('a, 'b, 'c) Event.t list
  end
