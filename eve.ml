open Core.Std
open Common
open Clock

module type EVENT = 
  functor (S:STATE)(T:TIME) -> sig
  type 'a t 
  val incrTime:  S.t -> S.t * T.t e
end

module type EVENTLIST = sig
  type a' b' t


module SimpList = struct
  type a' b' t = 
