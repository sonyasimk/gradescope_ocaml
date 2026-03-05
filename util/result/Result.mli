type 'a result = Value of 'a | Raise of exn | Timeout of int
type 'a t = 'a result

val evaluate : int -> ('a -> 'b) -> 'a -> 'b t

(* monadic operations *)
val return  : 'a -> 'a t
val compose : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t

val bind : 'a t -> ('a -> 'b t) -> 'b t
val seq  : 'a t -> 'b t -> 'b t
val join : 'a t t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t

exception Result
val valOf : 'a t -> 'a

val toString : ('a -> string) -> 'a t -> string