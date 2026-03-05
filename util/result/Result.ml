open FormatUtil

type 'a result = Value of 'a | Raise of exn | Timeout of int

type 'a t = 'a result

(* https://discuss.ocaml.org/t/computation-with-time-constraint/5548 *)
exception TimeoutExn

let evaluate timeout f x =
  let _ =
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise TimeoutExn))
  in
  ignore (Unix.alarm timeout);
  try
    let r = f x in
    ignore (Unix.alarm 0); Value r
  with
  | TimeoutExn  -> ignore (Unix.alarm 0); Timeout timeout
  | e -> ignore (Unix.alarm 0); Raise e

let map f = function
  Value x   -> Value (f x)
| Raise e   -> Raise e
| Timeout t -> Timeout t

let return x = Value x

let mapPartial f = function
  Value x   -> f x
| Raise e   -> Raise e
| Timeout t -> Timeout t

let (>>=) x f = mapPartial f x

let compose f g = mapPartial g >> f
let bind = (>>=)
let seq x y = x >>= Fun.const y
let join x = x >>= Fun.id


exception Result
let valOf = function
  Value x -> x
| _ -> raise Result

let toString f = function
  Value x   -> f x
| Raise e   -> "uncaught exception " ^ Printexc.to_string e
| Timeout t -> "timed out after " ^ string_of_int t ^ "s"