open GraderSig
open OutputSig
module type BUCKET_INPUT = GeneralizedBucketGrader.BUCKET_INPUT

module type EQUIV_BUCKET_INPUT =
  sig
    val description : string

    type input
    module Output : OUTPUT

    module Bucket : OUTPUT

    type test = {
      name : string;
      bucket : Bucket.t;
      gen : input QCheck.arbitrary;
      numTests : int;
      timeout : int
    }
    val tests : test list
    val buckets : (Bucket.t * int) list

    val refsol     : input -> Output.t
    val submission : input -> Output.t
  end

module EquivAuxBucket (I : EQUIV_BUCKET_INPUT) : BUCKET_INPUT =
  struct
    include I

    type property = {
      name        : string;
      bucket      : Bucket.t;
      showInput   : unit -> string;
      showOutput  : input -> unit -> string;
      gen         : input QCheck.arbitrary;
      check       : input -> Output.t -> bool;
      numTests    : int;
      timeout     : int
    }

    let properties : property list = List.map (fun (t : test) -> 
      let runRefsol i = Result.valOf (Result.evaluate t.timeout refsol i) in
      let check i o = Output.equal (runRefsol i) o 
      in
        {
          name = "Checking " ^ t.name;
          bucket = t.bucket;
          showInput = (fun () -> t.name);
          showOutput = (fun i () -> Output.show (runRefsol i));
          gen = t.gen;
          check = check;
          numTests = t.numTests;
          timeout = t.timeout
        }) tests
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