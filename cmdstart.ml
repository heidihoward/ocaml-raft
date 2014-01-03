open Core.Std
open Common

let run ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max ~debug ~iter
  ~data ~real =
  let module Par = (struct
    let nodes = nodes
    let timeout = function
      | Leader -> NumberGen.fixed 5 ()
      | Follower | Candidate -> NumberGen.uniform_int time_min time_max ()
    let pkt_delay = NumberGen.uniform_int delay_min delay_max
    let termination = term 
    let debug_mode = debug
    let write_data _ = ignore(data);()
    (* TODO implement the write data for leader election tests
      match data with 
      | Some file -> () 
      | None -> () *)
  end : PARAMETERS) in 
   if (real) then begin
  let module DES =  
    Des.DEventSim(IntID)(Clock.RealTime)(LogEntry)(ListLog)(Par) in
  for i=1 to iter do ignore(i); DES.start() done end 
   else begin
  let module DES =  
    Des.DEventSim(IntID)(Clock.FakeTime)(LogEntry)(ListLog)(Par) in
  for i=1 to iter do ignore(i); DES.start() done end

let command =
  Command.basic 
    ~summary:"Discrete Event Simulator & Realtime Simulator for Raft's Leader Election"
    ~readme: (fun () -> "see www.cl.cam.ac.uk/~hh360 for more information ")
    Command.Spec.(
      empty
      +> flag "-nodes" (required int) 
        ~doc:"int Number of nodes to simulate"
      +> flag "-term" (optional_with_default 5000 int)
        ~doc:"int The maxiumun time before termination"
      +> flag "-time-min" (optional_with_default 100 int)
        ~doc:"int The minimum timeout used"
      +> flag "-time-max" (optional_with_default 150 int)
        ~doc:"int The max timeout used"
      +> flag "-delay-min" (optional_with_default 6 int)
        ~doc:"int The min packet delay"
      +> flag "-delay-max" (optional_with_default 8 int)
        ~doc:"int The max delay of packets"
      +> flag "-d" no_arg
        ~doc:"Enable debug (disabled by default)"
      +> flag "-iter" (optional_with_default 1 int) 
        ~doc:"int Number of Simulations"
      +> flag "-data" (optional string) 
        ~doc:"filename File to output data to as .data"
      +> flag "-r" no_arg 
        ~doc:"Run as simulation in realtime instead of as a DES"
    )
    (fun nodes term time_min time_max delay_min delay_max debug iter data real () -> 
      run ~nodes ~term ~time_min ~time_max ~delay_min ~delay_max ~debug ~iter
      ~data ~real)

let () =  Command.run command
