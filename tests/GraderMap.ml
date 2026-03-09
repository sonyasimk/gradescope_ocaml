open GraderSig

let get problem task =
  match problem with
    "sum" -> 
      (match task with
        "sum" -> (module SumGrader.Grade : GRADER))