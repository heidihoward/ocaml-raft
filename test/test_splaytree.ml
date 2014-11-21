open Core_kernel.Std
open OUnit

module TestingList = (Splaytree.SplayTree(Int) : Eventlst.EVENTLIST with type item = int)

let to_list testList =
  let rec to_list_sub xs resultList =
        match TestingList.hd xs with
          None -> resultList
        | Some(x,ys) -> to_list_sub ys (x::resultList)

  in List.rev (to_list_sub testList [])

let test_init _ = 
        assert_equal ~msg:"init empty list failed" None (TestingList.hd (TestingList.init []));
        let hd = (TestingList.hd (TestingList.init [100;8;5;43;27]))
        in (match hd with
             Some(x,_) -> assert_equal ~msg:"init with multiple element list
             failed" 5 x
           | _ -> assert false );
        assert_equal ~msg:"init multiple element list not equal"
           [5;12;98;345;736;897] (to_list (TestingList.init [736;345;897;12;98;5]))
        let hd = (TestingList.hd (TestingList.init [4]))
        in (match hd with
             Some(x,y) -> assert_equal ~msg:"init with single element failed" (4,None) (x,TestingList.hd y)
           | _ -> assert false)

let lst = TestingList.init [3567;23;8;123;9;3]

let test_add _ =
        assert_equal ~msg:"adding empty list failed" lst (TestingList.add [] lst);
        assert_equal ~msg:"adding one element failed" [3;8;9;23;42;123;3567]
        (to_list (TestingList.add [42] lst));
        assert_equal ~msg:"adding multiple elements failed" [3;6;9;23;65;76;123]
        (to_list (TestingList.add [23;6;123] (TestingList.init [9;3;76;65])))
        
let suite = "Test SplayTree" >:::
        ["Test init" >:: test_init;
        "Test add" >:: test_add]

let _ =
  run_test_tt_main suite
