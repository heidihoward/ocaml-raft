open Core.Std
open Async.Std

let msg_log () = Writer.open_file "msg.log"

let append_log w msg = w >>> (fun w -> Writer.write_line w msg)
