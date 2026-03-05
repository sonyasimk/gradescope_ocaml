(* to be used with ppx_deriving, whenever possible *)
module type OUTPUT =
  sig
    type t
    val show : t -> string
    val equal : t -> t -> bool
  end