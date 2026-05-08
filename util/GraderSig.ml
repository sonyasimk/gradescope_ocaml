module type GRADER =
  sig
    module Rubric :
      sig
        val description : string

        type t
        val toString : t -> string
        val score : t -> Q.t
      end

    val process : unit -> Rubric.t
  end
