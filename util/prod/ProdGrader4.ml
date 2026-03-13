open GraderSig
open FormatUtil

module Make (I : sig
  val description : string
  module Grader1 : GRADER
  module Grader2 : GRADER
  module Grader3 : GRADER
  module Grader4 : GRADER
  val weights : int * int * int * int
end) : GRADER =
  struct
    open I

    module Rubric =
      struct
        let description = description

        type t = {
          g1 : Grader1.Rubric.t;
          g2 : Grader2.Rubric.t;
          g3 : Grader3.Rubric.t;
          g4 : Grader4.Rubric.t
        }

        let scores = fun {g1=g1;g2=g2;g3=g3;g4=g4} -> [
          Grader1.Rubric.score g1;
          Grader2.Rubric.score g2;
          Grader3.Rubric.score g3;
          Grader4.Rubric.score g4
        ]

        let fractions =
          let (w1,w2,w3,w4) = weights in
          let weights = List.map Q.of_int [w1;w2;w3;w4] in
          let total = List.fold_left (Q.add) Q.zero weights in
            List.map
              (match Q.compare total Q.zero with
                0 -> Fun.const Q.one
              | _ -> (Fun.flip Q.div) total)
              weights

        let toString ({g1;g2;g3;g4} as rubric) =
          let descriptions = [
            Grader1.Rubric.description;
            Grader2.Rubric.description;
            Grader3.Rubric.description;
            Grader4.Rubric.description
          ] in
          let combine percent description = percent ^ " " ^ description in
          let format description output = description ^ "\n" ^ FormatUtil.indent output in
            String.concat "" (
              List.map2 format
                (List.map2 combine (
                  List.map2 FormatUtil.showPercents
                    (scores rubric)
                    fractions
                    )
                  descriptions
                )
                [
                  Grader1.Rubric.toString g1;
                  Grader2.Rubric.toString g2;
                  Grader3.Rubric.toString g3;
                  Grader4.Rubric.toString g4
                ]
            )

        let score =
          List.fold_left Q.add Q.zero
          >> List.map2 Q.mul fractions
          >> scores
      end

    let process () : Rubric.t = {
      g1 = Grader1.process ();
      g2 = Grader2.process ();
      g3 = Grader3.process ();
      g4 = Grader4.process ()
    }
  end