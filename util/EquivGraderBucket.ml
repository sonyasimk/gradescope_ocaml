open GraderSig
open OutputSig

module type EQUIV_BUCKET_INPUT =
  sig
    val description : string

    type input
    val show_input : input -> string
    module Output : OUTPUT

    module Bucket : OUTPUT

    type test = {
      bucket : Bucket.t;
      gen : input QCheck.arbitrary;
      numTests : int;
      filter : (input -> bool) option;
      timeout : int
    }
    val tests : test list
    val buckets : (Bucket.t * int) list

    val refsol     : input -> Output.t
    val submission : input -> Output.t
  end

module EquivAuxBucket (I : EQUIV_BUCKET_INPUT) =
  struct
    include I

    type property = {
      name        : string;
      bucket      : Bucket.t;
      showInput   : input -> string;
      showOutput  : input -> string;
      gen         : input QCheck.arbitrary;
      check       : input -> Output.t -> bool;
      filter      : (input -> bool) option;
      numTests    : int;
      timeout     : int
    }

    let properties : property list = List.map (fun (t : test) -> 
      let runRefsol i = Result.valOf (Result.evaluate t.timeout refsol i) in
      let check i o = Output.equal (runRefsol i) o 
      in
        {
          name = "Checking submission against refsol";
          bucket = t.bucket;
          showInput = (fun i -> show_input i);
          showOutput = (fun i -> Output.show (runRefsol i));
          gen = t.gen;
          check = check;
          filter = t.filter;
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