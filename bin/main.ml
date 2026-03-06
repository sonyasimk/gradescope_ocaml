open Grader
open FormatUtil
module G = SumGrader.Grade

let _ = (
  let filename = 
    try Unix.getenv "GRADER_SCORES_FILE" with Not_found -> failwith "GRADER_SCORES_FILE not defined" in
  let problemName = 
    try Unix.getenv "GRADER_PROBLEM_NAME" with Not_found -> failwith "GRADER_PROBLEM_NAME not defined" in
  let maxScore = 
    (match (try int_of_string_opt (Unix.getenv "GRADER_MAX_SCORE") with Not_found -> failwith "GRADER_MAX_SCORE not defined") with
      None -> failwith "GRADER_MAX_SCORE is an invalid int"
    | Some n -> n) in
  let rubric = G.process () in
  (* compute numerator and denominator of rubric instance score *)
  let score =
    let q = G.Rubric.score rubric in
    if Q.(<) q Q.zero then failwith "Rational less than 0";
    if Q.(>) q Q.one then failwith "Rational greater than 1";
    let (num, den) = (Z.to_int (Q.num q), Z.to_int (Q.den q)) in
      (float_of_int (maxScore * num)) /. (float_of_int den) in

  let json = Yojson.Safe.from_file filename in
  (* effectively "output.json" *)
  let entry : Yojson.Safe.t = 
    `Assoc [
      ("name", `String problemName);
      ("score", `Float score);
      ("max_score", `Int maxScore);
      ("output", `String (G.Rubric.toString rubric))
    ] in 
  let newEntry = 
    (match json with
    `Assoc fields ->
      (match List.find_opt (String.equal "tests" >> fst) fields with
        Some (_, `List l) -> `List (l @ [entry])
      | None ->  `List [entry]
      | _ -> failwith "`tests` field in result JSON file is ill-formatted")
    | _ -> failwith "Result JSON file does not have proper formatting") in
  Yojson.Safe.to_file filename (`Assoc [("tests", newEntry)])
)