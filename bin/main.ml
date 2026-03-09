open Tests
open FormatUtil

let rec processTasks problem tasksAndScores filename =
  match tasksAndScores with
    [] -> ()
  | (t, s)::ts ->
    let module G = (val GraderMap.get problem t) in
      let rubric = G.process () in
      (* compute numerator and denominator of rubric instance score *)
      let score =
        let q = G.Rubric.score rubric in
        if Q.(<) q Q.zero then failwith "Rational less than 0";
        if Q.(>) q Q.one then failwith "Rational greater than 1";
        let (num, den) = (Z.to_int (Q.num q), Z.to_int (Q.den q)) in
          (float_of_int (s * num)) /. (float_of_int den) in

      let json = Yojson.Safe.from_file filename in
      (* effectively "output.json" *)
      let entry : Yojson.Safe.t = 
        `Assoc [
          ("name", `String (Printf.sprintf "%s: %s" problem t));
          ("score", `Float score);
          ("max_score", `Int s);
          ("output", `String (G.Rubric.toString rubric))
        ] in 
      let newEntry = 
        (match json with
        `Assoc fields ->
          (match List.find_opt (String.equal "tests" >> fst) fields with
            Some (_, `List l) -> `List (l @ [entry])
          | None ->  `List [entry]
          | _ -> failwith "`tests`maxScore field in result JSON file is ill-formatted")
        | _ -> failwith "Result JSON file does not have proper formatting") in
      Yojson.Safe.to_file filename (`Assoc [("tests", newEntry)]); 
      processTasks problem ts filename

let _ = (
  let filename = 
    try Unix.getenv "GRADER_SCORES_FILE" with Not_found -> failwith "GRADER_SCORES_FILE not defined" in
  let problemName = 
    try Unix.getenv "GRADER_PROBLEM_NAME" with Not_found -> failwith "GRADER_PROBLEM_NAME not defined" in
  let tasks = 
    try String.split_on_char ' ' (Unix.getenv "GRADER_PROBLEM_TASKS") with Not_found -> failwith "GRADER_PROBLEM_TASKS not defined" in
  let taskScores = 
    (let scoreStrings = try String.split_on_char ' ' (Unix.getenv "GRADER_TASK_SCORES") with Not_found -> failwith "GRADER_TASK_SCORES not defined" in
      try List.map int_of_string scoreStrings with _ -> failwith "Score in GRADER_TASK_SCORES is not an integer") in
  
  let tasksAndScores = List.combine tasks taskScores in
    processTasks problemName tasksAndScores filename
)