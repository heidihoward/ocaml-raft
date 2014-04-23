open Core.Std
open Common

  (*highest element is always at head *)
  type 'a t = (Index.t * Index.t * 'a) list with sexp
  let init () = []

  let append new_ele lst = 
   match new_ele,lst with
    | (new_index, new_term, new_cmd), [] -> assert(new_index= Index.succ (Index.init())); [new_ele]
    | (new_index, new_term, new_cmd), (index,term,cmd)::_ -> 
      assert(new_index = (Index.succ index)); 
      assert((Index.to_int index) = List.length lst);
      new_ele :: lst

  let appends elements lst = 
    match (List.last elements),lst with
    | None,_ -> lst
    | (Some new_ele ),_-> 
      let _ = append new_ele lst in
      elements @ lst

  let last_index_term = function 
  | [] -> ( Index.init(),Index.init() )
  | (i,t,_)::_ -> (i,t)

  let specific_index_term index lst =
  	match (List.find lst ~f:(fun (i,_,_) -> i=index)) with
  | Some (i,t,_) -> (i,t)
  (* dealing with the empty list case *)
  | None -> ( Index.init(), Index.init())

  let consistency_check log prev_index prev_term =
  if (prev_index = Index.init())&&(prev_term = Index.init()) then
    `Consistent
  else
    match List.find log ~f:(fun (index,_,_) -> (index=prev_index)) with
    | Some (_,term,_) when term=prev_term -> `Consistent
    | _ -> `Inconsistent

  let rec to_string ~cmd_to_string =
  	function
  	| [] -> "\n"
  	| (i,t,c)::rest -> (
  		"Index: "^(Index.to_string i)^
    	" Term: "^(Index.to_string t)^
    	" Cmd: "^(cmd_to_string c)^" |"^	
    	(to_string ~cmd_to_string rest))

  let to_commit old_index new_index log = 
    (*Log stores most recent at head but need to reply in rev order *)
    List.filter log ~f:( fun (x,_,_) -> (x > old_index) && (x <= new_index ) ) 
    |> List.sort ~cmp:(fun (x,_,_) (y,_,_) -> Index.compare x y) 
    |> List.map ~f:(fun (_,_,cmd) -> cmd)


  let cut_entries new_last_index log =
    List.filter log ~f:(fun (i,_,_) -> i <= new_last_index)

  let get_entries from_index log =
  	List.filter log ~f:(fun (i,_,_) -> i >= from_index)

