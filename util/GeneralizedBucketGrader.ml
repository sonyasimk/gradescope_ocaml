open GraderSig
open OutputSig
open FormatUtil

module Display =
  struct
    type nonrec string = unit -> string
    type 'a t = 'a * string  (* thunk *)
  end

module type BUCKET_INPUT =
  sig
    val description : string

    type input
    module Output : OUTPUT

    module Bucket : OUTPUT
    type property = {
      name        : string;
      bucket      : Bucket.t;
      showInput   : Display.string;
      showOutput  : Display.string;
      gen         : input QCheck.arbitrary;
      check       : input -> Output.t -> bool;
      numTests    : int;
      timeout     : int
    }
    val properties : property list
    val buckets : (Bucket.t * int) list

    val submission : input -> Output.t
  end

module type SCHEME =
  sig
    type 'a t
    val aggregate : ('a -> 'b option) -> 'a list -> 'b t

    val score : 'a t -> Q.t
    val toString : ('a -> string) -> 'a t -> string
  end

module Make 
  (Input : BUCKET_INPUT)
  (Scheme : SCHEME) : GRADER =
  struct
    module Rubric =
      struct
      let description = Input.description

      type tests = {
        input    : Display.string; 
        expected : Display.string;
        output   : Input.Output.t Result.t
        }
      type t = tests Scheme.t list
      (* invariant: length = List.length buckets *)

      let scores : t -> Q.t list = List.map Scheme.score

      let fractions =
        let weights = List.map (fun (_, b) -> Q.of_int b) Input.buckets in
        let total = List.fold_left (Q.add) Q.zero weights in
          List.map
            (match Q.compare total Q.zero with
              0 -> Fun.const Q.one
            | _ -> (Fun.flip Q.div) total)
            weights

      let combine percent description = percent ^ " " ^ description
      let format description output = description ^ "\n" ^ FormatUtil.indent output

      let schemeToString =
        Scheme.toString (fun { input; expected; output } ->
          "Test failed:\n" ^
          FormatUtil.indentWith "  Test    : " (input ()) ^ "\n" ^
          FormatUtil.indentWith "  Expected: " (expected ()) ^ "\n" ^
          FormatUtil.indentWith "  Received: " (Result.toString Input.Output.show output) ^ "\n"
        )

      let toString =
      function
        []      -> "No tests run.\n"
      | [x] -> schemeToString x
      | rubric ->
        String.concat "" (
          List.map2 format (
            List.map2 combine (
              List.map2 FormatUtil.showPercents
                (scores rubric)
                fractions
              )
              (List.map (Input.Bucket.show >> fst) Input.buckets))
            (List.map schemeToString rubric)
        )

      let score =
        List.fold_left Q.add Q.zero
        >> List.map2 Q.mul fractions
        >> scores
    end

    let rec partition p =
      function
        []     -> (fun _ -> [])
      | x :: xs -> (
          let f = partition p xs
          in
            fun y -> if Input.Bucket.equal y (p x) then x :: f y else f y
        )
    
    let propertyBuckets = partition (fun (p : Input.property) -> p.bucket) Input.properties

    let checkProp submission (prop : Input.property) =
      let res = ref None in
      let test =
        QCheck.Test.make_cell
          ~name:prop.name
          ~count:prop.numTests
          prop.gen
          (fun input ->
            let r = Result.evaluate prop.timeout submission input in
              match r with
              | Result.Value v -> 
                if prop.check input v then true
                else (res := Some (Result.Value v, prop.showInput, prop.showOutput); false)
              | _ -> res := Some (r, prop.showInput, prop.showOutput); false)
      in
        (match QCheck2.TestResult.get_state (QCheck2.Test.check_cell test) with
          QCheck2.TestResult.Success -> None
        (* other options include Failed, Failed_other, and Error *)
        | _ -> !res)

    let process =
      let processBucket =
        Scheme.aggregate (fun p ->
          let resultOpt = checkProp Input.submission p in
          let f (output, input, expected) : Rubric.tests = { input = input; expected = expected; output = output } in
            Option.map
              f
              resultOpt
        )
    in 
      fun () -> List.map (processBucket >> propertyBuckets >> fst) Input.buckets
  end