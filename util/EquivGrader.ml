open GraderSig

module type EQUIV_INPUT =
  sig
    val description : string

    type input

    type output
    [@@deriving show, eq]

    val tests : (string * input) list
    (* val timeout : Time.time *)
    val timeout : float

    val refsol     : input -> output
    val submission : input -> output
  end

module EquivAux (I : EQUIV_INPUT) =
  struct
    include I

    type bucket = unit
    [@@deriving show, eq]
    let bucket = ignore
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