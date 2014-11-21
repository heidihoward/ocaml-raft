open Core.Std
open Common
open MonoTime
module RaftMonitor = RaftMonitorWrapper
module Mach = Statemach.KeyValStr
open State
  
let hist = ref false

type t = (IntID.t,State.t status list) List.Assoc.t
(*this is only created by init so we are assuming list always have at least
 * one element, hence use of hd_exn*)

let append sl id (s: State.t status) = 
if !hist then
  List.Assoc.add sl id (s::(List.Assoc.find_exn sl id))
else
  List.Assoc.add sl id [s]

let find sl id  = match (List.Assoc.find sl id) with
  | Some x -> List.hd_exn x
  | None -> Notfound

  (*like find but also strips status *)
let find_wst sl id = match (find sl id) with
  | Live x | Down x -> x | Notfound -> exit 1

(* returns a list of state.t for all live nodes, useful for iterating over *)
let get_live (sl:t) = 
  let f (_,s) = (match (List.hd_exn s) with
  | Live _ -> true 
  | Down _ | Notfound -> false ) in
  let live_list = List.filter sl ~f in
  let g (_,s) = (match (List.hd_exn s) with Live st -> st) in
  List.map live_list ~f:g

let leader_agreed sl = 
  let (live: State.t list) = get_live sl in
  (* check majority of nodes are live *)
  if ((List.length live)*2 > (List.length sl)) then
    match live with | hd::_ ->
    match hd.leader with 
    | None -> false  (* if hd doesn't know leader *)
    | Some _ ->
        (* check all live nodes agree on leader and term *)
        (* TODO: this condition is too strong *)
    List.for_all live ~f:(fun s -> s.leader=hd.leader && s.term=hd.term)
  else false

let exists sl id = List.exists sl ~f:(fun (node_id,_) -> node_id=id) 

let add sl id state = 
  match (find sl id) with
  | Notfound -> assert false
  | Live _ -> (* adding new node, assume live or already live *) 
      append sl id (Live state)
  | Down _ -> (* updating a dead node *)
          exit 1

(*  let from_listassoc x = x *)

let init n store_hist possible_leaders : t =
  let id_list = List.init n ~f:(IntID.from_int) in
  let remove x xs = List.filter xs ~f:(fun y -> not (x = y)) in 
  let gen_state id id_list = 
    State.init id (remove id id_list) (IntID.to_int(id)<=possible_leaders) in
  hist := store_hist; 
  List.map 
  ~f:(fun node_id -> node_id , [(Live (gen_state node_id id_list))] ) id_list

let check_condition sl ~f = 
  let f_new (a,b) = f (a, List.hd_exn b) in
  match (List.find sl ~f:f_new) with Some _ -> true | None -> false 

let kill sl id time= 
  match (find sl id) with
  | Live s -> 
      let s_new = State.tick (SetTime time) s in
      append sl id (Down s_new)
  | Down s -> assert false (* killing a down node *)

let wake sl id time =
  match (find sl id) with 
  | Down s -> 
      let s_new = 
        State.tick Restart s 
        |> State.tick (SetTime time)
        |> State.tick Set in
      append sl id (Live s_new)
  | Live s -> assert false (*waking a live node *)

let rec get_live_states sl = 
  let live_only l = List.filter_map l 
    ~f:(function Live x -> Some x | Down _ -> None) in
  match sl with
  | (id,s_lst)::rs -> (live_only s_lst) @ get_live_states rs
  | [] -> [] 

let lds_to_string lds = 
List.to_string lds  
  ~f:(fun (term,id) -> 
    " T: "^(Index.to_string term)^" L: "^(IntID.to_string id) )

let rec election_safety (states :State.t list) lds =
  match states with
  | s::rest -> ( match s.leader,(List.Assoc.find lds s.term) with
               | (Some ldx, Some ldy) when ldx=ldy -> 
                   election_safety rest lds
               | (None, _) -> election_safety rest lds    
               | (Some ldx, None) -> election_safety rest 
                   (List.Assoc.add lds s.term ldx) 
               | (Some ldx, Some ldy) -> 
                  printf "ELECTION SAFETY FAILED TERM %s LEADERS %s %s \n"
                    (Index.to_string s.term) (IntID.to_string ldx) (IntID.to_string ldy);
                  printf " %s \n %! " (lds_to_string lds); 
                   assert false )
  | [] -> lds_to_string lds



let check_safety sl =
  let states = get_live_states sl in
  let output = election_safety states [] in
  "Checking Election Safety ... \n"^output

let get_leader sl =
  match List.hd (get_live sl) with
  | None -> None
  | Some state ->
      match (state.leader) with
      | None -> None 
      | Some leader_id -> Some (find_wst sl leader_id)


