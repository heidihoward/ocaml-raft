open Core.Std
open Async.Std

type role = Follower | Candidate | Leader 

type log_entry = 
  { term: int;
    index: int;
    command: string; }

type state = 
  { candidate_id: int;
    mutable all_ids: int list;
    mutable leader_id: int option;
    mutable cnt_role: role;
    mutable cnt_term: int;
    mutable voted_for: int option;
    mutable log: log_entry list }

let create_persistent state =
   let file = (string_of_int state.candidate_id)^"log.log" in
   Writer.open_file file
   >>= (fun w -> Writer.write_line w (string_of_int state.cnt_term); Writer.close w) 
   

(*
let run_leader state = 
  (* this node has just won a eletion *) 
  send all appendEntries term:int leader_id:int prev_log_index:int prev_log_term:int (entries: log_entry list) commitIndex:int 

let requestVote term:int candidate_id:int last_log_index:int last_log_term:int = 
 >>= (fun w -> Writer.write_line w (string_of_int state.cnt_term); Writer.close w
let appendEntries term:int leader_id:int prev_log_index:int prev_log_term:int (entries: log_entry list) commitIndex:int =

let clientrequest command:string =
  match state.cnt_role with 
  |Follower -> return "Failure: please ask"^state.leader_id
  |Candidate -> return "Failure: try again later"
  |Leader -> *)

let rec msg_rcv state r = 
  let log_w = Msg.msg_log (string_of_int state.candidate_id) in
  Reader.read_line r 
  >>> (function 
    |`Ok msg ->  ignore(msg_rcv state r); 
      Msg.append_log log_w msg    
    |`Eof -> printf "oo";  ignore(msg_rcv state r) ) 

let test_msgs state w = 
  (*let timephase = Time.Span.create ~ms:20 () in 
  Clock.after timephase
  >>> (fun _ -> *)
    if (state.candidate_id=1) then 
    Writer.write w ("2:1:hello to number 2\n")
    else if (state.candidate_id=3) then 
    Writer.write w ("0:3:hello also to zero\n") 

let run ~id ~port = 
  (* assume this the first time candidate has been started up *)
   let state = { candidate_id =id; 
                all_ids=[];
		leader_id= None;
		cnt_role = Follower;
		cnt_term = 0;
		voted_for = None;
		log = []; } in 
  Tcp.connect (Tcp.to_host_and_port "localhost" port)
  >>| (fun (_,r,w) ->  msg_rcv state r;  
    Writer.write w ("SIM:"^(string_of_int state.candidate_id)^":hello\n");
    test_msgs state w )
  >>= (fun _ -> create_persistent state) 
  >>= (fun _ ->  Deferred.never () )
 
let () =
  Command.async_basic
    ~summary:"Start an raft node"
    Command.Spec.(
      empty
      +> flag "-id" (required int)
        ~doc:" candidate ID"
      +> flag "-port" (optional_with_default 8889 int)
        ~doc:" Port to listen on (default 8889)"
    )
    (fun id port () -> run ~id ~port)
  |> Command.run

