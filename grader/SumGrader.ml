module Grade = EquivGrader.Make (
  struct
    let description = "sum"
    type input = int list
    [@@deriving show]

    module Output = struct
      type t = int
      [@@deriving show, eq]
    end

    type eqTest = {
      gen : input QCheck.arbitrary;
      numTests : int;
      timeout : int
    }

    let test = { gen = QCheck.(list int); numTests = 200; timeout = 10 }

    let submission = Impl.Sum.sum
    let refsol = Refsol.Sum.sum
  end
  )
