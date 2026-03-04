open GraderSig
module type BUCKET_INPUT = GeneralizedBucketGrader.BUCKET_INPUT

module type EQUIV_BUCKET_INPUT =
  sig
    val description : string

    type input

    type output
    [@@deriving show, eq]

    val tests : (string * input) list
    (* val timeout : Time.time *)
    val timeout : float

    type bucket
    [@@deriving show, eq]
    val bucket : input -> bucket
    val buckets : (bucket * int) list

    val refsol     : input -> output
    val submission : input -> output
  end

module EquivAuxBucket (I : EQUIV_BUCKET_INPUT) : BUCKET_INPUT =
  struct
    include I

    let tests = List.map (fun (inputString,input) ->
      (* let refsolOutput = Result.valOf (Result.evaluate timeout refsol input) *)
      let refsolOutput = refsol input
      in
        ((input, fun () -> inputString), (
          equal_output refsolOutput,
          fun () -> show_output refsolOutput
        ))
    ) tests
  end

module Make (I : EQUIV_BUCKET_INPUT) : GRADER =
  BucketGrader.Make (EquivAuxBucket (I))

module MakeList (
  I :
    sig
      include EQUIV_BUCKET_INPUT
      val cutoff : int
    end
) : GRADER =
  BucketGrader.MakeList (
    struct
      module S = EquivAuxBucket (I)
      include S
      let cutoff = I.cutoff
    end
  )