module type BUCKET_INPUT = GeneralizedBucketGrader.BUCKET_INPUT
module type SCHEME = GeneralizedBucketGrader.SCHEME

module Make (I : BUCKET_INPUT) =
  GeneralizedBucketGrader.Make
  (I)
  (struct
    type 'a t = 'a option

    let rec aggregate (f : 'a -> 'b option) : 'a list -> 'b t = function
      []     -> None
    | x :: xs -> (
        match f x with
          None   -> aggregate f xs
        | Some y -> Some y
      )

    let score = function
      None   -> Q.one
    | Some _ -> Q.zero

    let toString f = function
      None   -> "All tests passed.\n"
    | Some x -> f x
  end : SCHEME)

module MakeList (
  I :
    sig
      include BUCKET_INPUT
      val cutoff : int
    end
) =
  GeneralizedBucketGrader.Make 
  (I)
  (struct
    type 'a t = 'a list * int

    let rec aggregate (f : 'a -> 'b option) : 'a list -> 'b t = function
      []      -> ([], 0)
    | x :: xs -> (
        let (l, n) = aggregate f xs
        in
          match f x with
            None   -> (l, n + 1)
          | Some y -> (y :: l, n)
      )

    let score = function 
        ([] , 0) -> Q.one
      | (l  , n) -> Q.div (Q.of_int n) (Q.of_int (n + List.length l))

    let toString f (l, n) =
        "Tests passed: " ^ string_of_int n ^ "/" ^ string_of_int (n + List.length l)
        ^ " (" ^ FormatUtil.percent (score (l, n)) ^ ")\n"
        ^ String.concat "" (
          List.map
            (fun failedCase -> FormatUtil.indentWith "- " (f failedCase))
            (List.take (Int.min (List.length l) I.cutoff) l)
        ) ^ (if List.length l > I.cutoff then "- ...\n" else "")
   end : SCHEME)