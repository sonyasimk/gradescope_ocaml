module type GRADER =
  sig
    module Rubric :
      sig
        val description : string
        (* formerly "include SHOW" *)
        type t
        val toString : t -> string
        val score : t -> Q.t
      end

    val process : unit -> Rubric.t
  end