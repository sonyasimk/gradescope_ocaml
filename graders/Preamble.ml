open GraderSig

module Make (I : 
  sig
    val preamble : string
    module Grader : GRADER
  end
) =
  struct
    open I
    open Grader

    module Rubric =
      struct
        open Rubric
        let separator = String.concat "" (List.init 80 (fun _ -> "="))
        let toString rubric =
          preamble ^ "\n" ^ separator ^ "\n" ^ toString rubric
      end
  end