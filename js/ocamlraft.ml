open Core_kernel.Std

let run () = 
	if Int.is_positive 10 then 3 else 2

let _ = Js.Unsafe.global##run <- Js.wrap_callback run;