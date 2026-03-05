module type OUTPUT =
  sig
    type t
    val toString : t -> string
    val eq : t -> t -> bool
  end