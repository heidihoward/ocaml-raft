open Core.Std
open Async.Std

let msg_log name = let file = name^"msg.log" in Writer.open_file file
let append_log w msg = w >>> (fun w -> Writer.write_line w msg)
