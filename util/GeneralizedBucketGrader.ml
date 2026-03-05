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

    val tests : (input Display.t * (Output.t -> bool) Display.t) list
    val timeout : float
    (* val timeout : Time.time *)

    module Bucket : OUTPUT
    val bucket : input -> Bucket.t
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
        output   : (Input.Output.t, unit) Result.t
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

      (* TODO: get rid of this and make designated result library *)
      let resultToString r =
        match r with
          Ok o -> "Ok " ^ Input.Output.show o
        | Error _ -> "Error"

      let schemeToString =
        Scheme.toString (fun { input; expected; output } ->
          "Test failed:\n" ^
          FormatUtil.indentWith "  Test    : " (input ()) ^ "\n" ^
          FormatUtil.indentWith "  Expected: " (expected ()) ^ "\n" ^
          FormatUtil.indentWith "  Received: " (resultToString output) ^ "\n"
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
    
    let testBuckets = partition (Input.bucket >> fst >> fst) Input.tests

    (* let eval f = Result.evaluate timeout f *)
    let eval f x = f x

    let process =
      let processBucket =
      Scheme.aggregate (
        fun ((input, inputString), (p, pString)) ->
          let resultOpt =
              match Ok (eval Input.submission input) with
                Result.Ok x   -> if p x then None else Some (Result.Ok x)
              | Result.Error e   -> Some (Result.Error e)
              (* | Result.Timeout t -> SOME (Result.Timeout t) *)
          in
          let f output : Rubric.tests = { input = inputString; expected = pString; output = output } in
            Option.map
              f
              resultOpt
      )
    in 
      fun () -> List.map (processBucket >> testBuckets >> fst) Input.buckets
  end