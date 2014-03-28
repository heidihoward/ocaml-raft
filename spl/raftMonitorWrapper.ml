open Core.Std

exception Bad_statecall

type s = RaftMonitor.s
type t = RaftMonitor.t * (s list)

let s_to_string = function
  |`RestartElecton -> "restart election"
  |`Startup -> "startup"
  |`StepDown_from_Candidate -> "stepdown candidate"
  |`StepDown_from_Leader -> "stepdown leader"
  |`StartElecton -> "start election"
  |`WinElecton -> "win election"
  |`Recover -> "recover"


let init () = (RaftMonitor.init (), [])

let tick (monitor,history) statecall = 
	try
		let new_mon = RaftMonitor.tick monitor statecall in
		(new_mon, statecall::history)
	with
	Bad_statecall -> 
		printf "%s" (List.to_string ~f:s_to_string (statecall::history));
		raise Bad_statecall
