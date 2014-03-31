open Core.Std

exception Bad_statecall

type s = RaftMonitor.s
type t = RaftMonitor.t * (s list)

let s_to_string = function
  |`RestartElection -> "restart election"
  |`Startup -> "startup"
  |`StepDown_from_Candidate -> "stepdown candidate"
  |`StepDown_from_Leader -> "stepdown leader"
  |`StartElection -> "start election"
  |`WinElection -> "win election"
  |`Recover -> "recover"


let init () = (RaftMonitor.init (), [])

let tick (monitor,history) statecall = 
	try
		let new_mon = RaftMonitor.tick monitor statecall in
		printf "%s \n" (s_to_string statecall);
		(new_mon, statecall::history)
	with
	Bad_statecall -> 
		printf "%s \n %! " (List.to_string ~f:s_to_string (statecall::history));
		Out_channel.flush stdout;
		raise Bad_statecall
