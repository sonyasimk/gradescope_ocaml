open GraderSig

module Make (I : sig val description : string end) : GRADER =
  struct
    module Rubric = struct
      let description = I.description
      type t = unit
      let toString () = "Grading successful.\n"
      let score () = Q.one
    end

      let process () = ()
  end