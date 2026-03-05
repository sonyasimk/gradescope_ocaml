open OutputSig

(* to be used with ppx_deriving wherever convenient *)
module Make (I : 
  sig 
    type t 
    val show : t -> string 
    val equal : t -> t -> bool 
  end) : OUTPUT with type t = I.t =
  struct
    type t = I.t

    let toString = I.show
    let eq = I.equal
  end