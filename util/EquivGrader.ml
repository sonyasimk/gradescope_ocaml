open GraderSig
open OutputSig

module type EQUIV_INPUT =
  sig
    val description : string

    type input
    module Output : OUTPUT

    type eqTest = {
      name : string;
      gen : input QCheck.arbitrary;
      numTests : int;
      timeout : int
    }
    val test : eqTest

    val refsol     : input -> Output.t
    val submission : input -> Output.t
  end

module EquivAux (I : EQUIV_INPUT) =
  struct
    include I

    module Bucket : OUTPUT with type t = unit =
      struct
        type t = unit
        [@@deriving show, eq]
      end
    
    type test = {
      name : string;
      bucket : Bucket.t;
      gen : input QCheck.arbitrary;
      numTests : int;
      timeout : int
    }  

    let tests = [{ name = test.name; bucket = (); gen = test.gen; numTests = test.numTests; timeout = test.timeout }]
    
    let buckets = [((), 1)]
  end

module EquivGrader (I : EQUIV_INPUT) : GRADER =
  EquivGraderBucket.Make (EquivAux (I))

module EquivGraderList (I :
    sig
      include EQUIV_INPUT
      val cutoff : int
    end
) : GRADER =
  EquivGraderBucket.MakeList (
    struct
      module S = EquivAux (I)
      include S
      let cutoff = I.cutoff
    end
  )