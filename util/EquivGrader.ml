open GraderSig
open OutputSig

module type EQUIV_INPUT =
  sig
    val description : string

    type input
    module Output : OUTPUT

    type eqTest = {
      gen : input QCheck.arbitrary;
      numTests : int;
      timeout : int;
      toString : input -> string
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
      bucket : Bucket.t;
      gen : input QCheck.arbitrary;
      numTests : int;
      timeout : int;
      toString : input -> string
    }  

    let tests = [{ bucket = (); gen = test.gen; numTests = test.numTests; timeout = test.timeout; toString = test.toString }]
    
    let buckets = [((), 1)]
  end

module Make (I : EQUIV_INPUT) : GRADER =
  EquivGraderBucket.Make (EquivAux (I))

module MakeList (I :
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