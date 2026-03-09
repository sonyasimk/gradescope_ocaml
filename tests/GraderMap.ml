open GraderSig

let get problem task =
  match problem with
    "sum" -> 
      (match task with
        "sum" -> (module SumGrader.Grade : GRADER)
      | _ -> failwith (Printf.sprintf "Task not defined: %s" task))
  | _ -> failwith (Printf.sprintf "Problem not defined: %s" problem)