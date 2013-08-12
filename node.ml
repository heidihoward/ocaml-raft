open Core.Std
open Async.Std
open Msg

(* type role = Follower | Candidate | Leader 

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
    mutable log: log_entry list } *)

(* let create_persistent state =
   let file = (string_of_int state.candidate_id)^"log.log" in
   Writer.open_file file
   >>= (fun w -> Writer.write_line w (string_of_int state.cnt_term); Writer.close w) *)
   
(*
let run_leader state = 
  (* this node has just won a eletion *) 
  send all appendEntries term:int leader_id:int prev_log_index:int prev_log_term:int (entries: log_entry list) commitIndex:int 

let requestVote term:int candidate_id:int last_log_index:int last_log_term:int = 
 >>= (fun w -> Writer.write_line w (string_of_int state.cnt_term); Writer.close w
let appendEntries term:int leader_id:int prev_log_index:int prev_log_term:int (entries: log_entry list) commitIndex:int =

  (* assume this the first time candidate has been started up *)
   let state = { candidate_id =id; 
                all_ids=[];
		leader_id= None;
		cnt_role = Follower;
		cnt_term = 0;
		voted_for = None;
		log = []; } in 

let clientrequest command:string =
  match state.cnt_role with 
  |Follower -> return "Failure: please ask"^state.leader_id
  |Candidate -> return "Failure: try again later"
  |Leader -> *)
(*  >>= (fun _ -> create_persistent state) *)


let debug_active = ref true

let debug x = if !debug_active then 
  let time = Time.Ofday.to_sec_string (Time.Ofday.now()) in
  (printf "[%s] %s \n %!" time x)


let rec msg_rcv  r log_w = 
  Reader.read_line r 
  >>> (function 
    |`Ok msg ->  debug ("MSG incoming: "^(Msg.to_string msg)); Msg.append_log log_w msg; msg_rcv  r log_w 
    |`Eof -> debug "EoF has been read from socket"; msg_rcv r log_w ) 

let msg_snd id pkt_w log_w too msg =
  let msg_pkt = too^":"^(string_of_int id)^":"^msg in
  debug ("MSG outgoing: "^(Msg.to_string msg_pkt));
  Msg.append_log log_w msg_pkt; 
  Writer.write_line pkt_w msg_pkt

let rec test_msgs id pkt_w log_w= 
  let timephase = Time.Span.randomize (Time.Span.create ~sec:3 ()) ~percent:0.75 in 
  let rcv = string_of_int (Random.int 10) in
  Clock.after timephase
  >>> (fun _ -> 
  msg_snd id pkt_w log_w rcv "random hello msg"; 
  test_msgs id pkt_w log_w )

let run ~id ~port = 
  debug "starting up";
  Random.self_init ();
  Tcp.connect (Tcp.to_host_and_port "localhost" port)
  >>| (fun (_,pkt_r,pkt_w) ->  
    debug "connected to simulator";
    let log_w = Msg.msg_log (string_of_int id) in
    msg_rcv pkt_r log_w;  
    msg_snd id pkt_w log_w "SIM" "hello";
    test_msgs id pkt_w log_w )
  >>= (fun _ ->  Deferred.never () )
 
let () =
  Command.async_basic
    ~summary:"Start an network node"
    Command.Spec.(
      empty
      +> flag "-id" (required int)
        ~doc:" candidate ID"
      +> flag "-port" (optional_with_default 8889 int)
        ~doc:" Port to listen on (default 8889)"
    )
    (fun id port () -> run ~id ~port)
  |> Command.run

