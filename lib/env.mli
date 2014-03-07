open Core.Std
open Common

(**this is the standard State Handler*)
(* module StateHandler : Envsig.STATEHANDLER *)

(**this is a modified State Handler which also stores the history of all states, ready for output and assertion checking *)
module StateHandlerHist : Envsig.STATEHANDLER
