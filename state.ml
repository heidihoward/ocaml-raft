open Core.Std
open Async.Std

type t = 
  {mutable id: int option; 
   mutable all_ids: (string * int) Int.Table.t;
   mutable leader_id: int option;
   mutable cnt_role: Role.t;
   mutable cnt_term: int;
   mutable voted_for: int option;
   log: Lg.t }

let empty = 
  {id=None;
   all_ids=Int.Table.create ~size:10 () ;
   leader_id=None;
   cnt_role=Role.start_up;
   cnt_term=0;
   voted_for= None; 
   log=Lg.start }

let set_id t new_id = 
  t.id <- Some new_id

let get_id t = match t.id with
  | None -> 0
  | Some x -> x

let add_nodes t nodes = 
  let add (id,addr) = Hashtbl.set t.all_ids ~key:id ~data:addr in 
  List.iter ~f:add nodes

let get_addr_by_id t id = Hashtbl.find t.all_ids id

let get_addr_all t = Hashtbl.to_alist t.all_ids
