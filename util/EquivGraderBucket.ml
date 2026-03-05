open GraderSig
open OutputSig

module type EQUIV_BUCKET_INPUT =
  sig
    val description : string

    type input
    module Output : OUTPUT

    val tests : (string * input) list
    (* val timeout : Time.time *)
    val timeout : float

    module Bucket : OUTPUT
    val bucket : input -> Bucket.t
    val buckets : (Bucket.t * int) list

    val refsol     : input -> Output.t
    val submission : input -> Output.t
  end

module EquivAuxBucket (I : EQUIV_BUCKET_INPUT) =
  struct
    include I

    let tests = List.map (fun (inputString,input) ->
      (* let refsolOutput = Result.valOf (Result.evaluate timeout refsol input) *)
      let refsolOutput = refsol input
      in
        ((input, fun () -> inputString), (
          Output.equal refsolOutput,
          fun () -> Output.show refsolOutput
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