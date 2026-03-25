open FormatUtil

type 'a result = Value of 'a | Raise of exn | Timeout of int

type 'a t = 'a result

let evaluate timeout f x =
  try Value (Util.prop_timeout timeout f x) with
    Util.Timeout -> Timeout timeout
  | e -> Raise e

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