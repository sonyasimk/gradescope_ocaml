module Grade = EquivGrader.Make (
  struct
    let description = "sum"
    type input = int list
    module Output = struct
      type t = int
      [@@deriving show, eq]
    end

    type eqTest = {
      gen : input QCheck.arbitrary;
      numTests : int;
      timeout : int;
      toString : input -> string
    }

    (* temp *)
    let int_list list = "[" ^ String.concat "; " (List.map string_of_int list) ^ "]"
    let test = { gen = QCheck.(list int); numTests = 200; timeout = 10; toString = int_list }

    let submission = Impl.Sum.sum
    let refsol = Refsol.Sum.sum
  end
  )
