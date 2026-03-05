open Grader

let () = (
  let rubric = SumGrader.Grade.process () in

  let message = FormatUtil.indentWith "" (SumGrader.Grade.Rubric.toString rubric) in

  (* compute numerator and denominator of rubric instance score *)
  let (a, b) = 
    let q = SumGrader.Grade.Rubric.score rubric in (Z.to_int (Q.num q), Z.to_int (Q.den q)) in

  (* write results to output.json *)
  let json =
    `Assoc [
      ("message", `String message);
      ("score", `List [`Int a; `Int b])
    ] 
  in
  
  Yojson.Safe.to_file "output.json" json
)