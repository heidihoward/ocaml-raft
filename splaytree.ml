open Core.Std


module SplayTree (Item: sig type t with compare end) = struct

  type item = Item.t
  type t = Leaf | Node of t * item * t

  let empty () = Leaf;;


  let rec splay (x, tree) =
    match tree with
      (** If tree is empty no splaying needed **)
      Leaf -> Leaf
    | Node (l,y,r) ->
        (match Item.compare x y with
        (** if item is at root no splaying needed **)
          0 -> tree
        | -1 -> 
            (match l with
              Leaf -> tree
            | Node (ll,z,rr) ->
               (match Item.compare x z with
                 0 -> Node (ll,z, Node(rr,y,r))
               | -1 ->
                  (match ll with
                    Leaf -> Node (ll,z, Node(rr,y,r))
                  | _ ->
                     let Node (newL, newV, newR) = splay (x,ll)
                     in Node (newL,newV, Node (newR,z, Node (rr,y,r))))
               | 1 ->
                   (match rr with
                     Leaf -> Node (ll, z, Node(rr,y,r))
                   | _ ->
                     let Node (newL, newV, newR) = splay (x,rr)
                     in Node (Node (ll,z,newL),newV,Node (newR,y,r)))))
        | 1 ->
            (match r with
              Leaf -> tree
            | Node (ll,z,rr) ->
               (match Item.compare x z with
                 0 -> Node ( Node (l,y,ll),z,rr)
               | -1 ->
                  (match ll with
                    Leaf -> Node ( Node (l,y,ll),z,rr)
                  | _ ->
                     let Node (newL, newV, newR) = splay (x,ll)
                     in Node (Node (l,y,newL),newV,Node (newR,z,rr)))
               | 1 ->
                  (match rr with
                    Leaf -> Node ( Node (l,y,ll),z,rr)
                  | _ -> 
                     let Node (newL,newV,newR) = splay (x,rr)
                     in Node (Node (Node (l,y,ll),z,newL),newV,newR)))))

  let insert (x,tree) =
    match tree with
      (** an empty tree becomes the itme with 2 leaves **) 
      Leaf -> Node (Leaf,x,Leaf)
    | _ ->
       let Node (l,y,r) = splay (x,tree)
       in
         (match Item.compare x y with
           0 -> Node (l,y,r)
         | -1 -> Node (l,x, Node (Leaf,y,r))
         | 1 -> Node (Node (l,y,Leaf),x,r))

  let rec add xs tree =
    match xs with
      [] -> tree
    | x::ys -> add ys (insert (x,tree))

  let init init_events = add init_events (empty ())

  let delete (x,tree) =
    match tree with
      Leaf -> Leaf
    | _ ->
        let Node (l,y,r) = splay (x,tree)
        in
          (match Item.compare x y with
            0 ->
              (match (l,r) with
                (Leaf,_) -> r
              | (_,Leaf) -> l
              | (_,_) ->
                  let Node (newL,newV,newR) = splay (x,l)
                  in Node (newL,newV,r))
          | _ -> Node (l,y,r))

 let rec min tree =
  match tree with 
    Leaf -> None
  | Node(Leaf,x,_) -> Some(x)
  | Node(l,_,_) -> min l

 let hd tree =
   match min tree with
     None -> None
   | Some(x) -> Some(x,delete(x,tree))

end

module TestingList = (SplayTree(Int) : Eventlst.EVENTLIST with type item = int)

let () =
  let print_hd lst = match TestingList.hd lst with | Some (x,lst_new) -> printf "%i \n" x; lst_new | None -> printf "empty \n"; lst in
  ( TestingList.init []
  |> TestingList.add [4]
  |> TestingList.add [6;50]
  |> TestingList.add [5]
  |> print_hd
  |> print_hd
  |> print_hd 
  |> print_hd
  |> print_hd
  |> (fun _ -> printf "done"))



