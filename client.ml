open Core.Std
open Common

module ClientHandler = 
  functor (MonoTime: Clock.TIME) ->
  functor (Mach: Statemach.MACHINE) -> struct

  (* for now we only have one client *)

  type t = { 
             workload : Mach.cmd list;
             time : unit -> MonoTime.t;
             allNodes : IntID.t list;
             leader : IntID.t option;
              }


  let init nodes =
            { workload = Mach.sample_workload; 
              time = MonoTime.init; 
              allNodes = List.map (List.range 1 nodes) ~f:IntID.from_int ; 
              leader = None} 


end