let (>>) f g = Fun.compose f g

let fst = fun (a, _) -> a

let percent =
  (Fun.flip (^)) "%"
  >> string_of_int
  >> (fun q -> (100 * Z.to_int (Q.num q)) / Z.to_int (Q.den q))

let showPercents score weight =
  let padLeft ch width s =
    let len = String.length s in
    if len >= width then
      s
    else
      String.make (width - len) ch ^ s in
  let padded = (padLeft ' ' 4) >> percent in
    "(" ^ padded (Q.mul weight score) ^
    "/" ^ padded weight ^ "):"

let indentWith s0 =
  let spaces = String.concat "" (List.init (String.length s0) (Fun.const " "))
  in
    String.concat ""
    >> List.mapi (
        fun i s ->
          (match i with
            0 -> s0
          | _ -> spaces)
          ^ s ^ "\n"
      )
    >> String.split_on_char '\n'

let indent = indentWith "  "
