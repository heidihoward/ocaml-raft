open Core.Std
open Common

module StateHandler =
  functor (State: Env.STATE) -> struct

  type t = (IntID.t,State.t status) List.Assoc.t

  let find sl id  = match (List.Assoc.find sl id) with
    | Some x -> x | None -> Notfound

  let find_wst sl id = match (find sl id) with
    | Live x | Down x -> x | Notfound -> exit 1

  (* returns a list of state.t for all live nodes, useful for iterating over *)
  let get_live sl = 
    let f (_,s) = (match s with
    | Live _ -> true 
    | Down _ | Notfound -> false ) in
    let live_list = List.filter sl ~f in
    let g (_,s) = (match s with Live st -> st) in
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
      List.for_all live ~f:(fun s -> s.leader=hd.leader && s.term=hd.term)
    else false
 
  let exists sl id = List.exists sl ~f:(fun (node_id,_) -> node_id=id) 

  let add sl id state = 
    match (find sl id) with
    | Notfound | Live _ -> (* adding new node, assume live or already live *) 
        List.Assoc.add sl id (Live state) 
    | Down _ -> (* updating a dead node *)
            exit 1
  
  let from_listassoc x = x

  let init n : t =
    let id_list = List.init n ~f:(IntID.from_int) in
    let remove x xs = List.filter xs ~f:(fun y -> not (x = y)) in 
    let gen_state id id_list = State.init id (remove id id_list) in
    List.map 
    ~f:(fun node_id -> node_id , (Live (gen_state node_id id_list))) id_list

  let check_condition sl ~f = 
    match (List.find sl ~f) with Some _ -> true | None -> false 

  let kill sl id time= 
    match (find sl id) with
    | Live s -> 
        let s_new = State.tick (SetTime time) s in
        List.Assoc.add sl id (Down s_new)
    | Down s -> exit 1 (* killing a down node *)

  let wake sl id time =
    match (find sl id) with 
    | Down s -> 
        let s_new = 
          State.tick Restart s 
          |> State.tick (SetTime time) in
        List.Assoc.add sl id (Live s_new)
    | Live s -> exit 1 (*waking a live node *)

end

