# Autograding Utilities

This file provides a detailed breakdown of how the autograding utilities work. 

## How it works

In this section, we will break down the general structure of the autograder and go into detail about the implementation of the grading functors (in particular, `GeneralizedBucketGrader.ml`). 

### [`GraderSig.ml`](/util/GraderSig.ml)

This is the signature for a task grader. It specifies a `Rubric` module, which describes the grading scheme for the task, and a `process` function, which, when applied, will grade a task according to the rubric.

The `Rubric` module is parameterized by a `type t` which (generally) represents the result of a test. The `score` function translates a result `t` into a corresponding score `Q.t` (the `zarith` type for rational numbers).
Since graders are defined per-task, a rubric only needs to describe the grading scheme for a particular task, not a whole problem. The individual task scores for a problem are aggregated together later in the [`main.ml`](/bin/main.ml).

### [`GeneralizedBucketGrader.ml`](/util/GeneralizedBucketGrader.ml)

This functor is the **only** direct implementation of the `GraderSig.GRADER` signature. All other functors (besides the ones in `TrivialGrader.ml` and `Preamble.ml`) are defined using this functor. This section will break down the individual components of the implementation.

The `Make` functor defined in this file takes in two structures ascribing to the following signatures: `BUCKET_INPUT` and `SCHEME`.

#### `BUCKET_INPUT`
---

The `BUCKET_INPUT` signature provides all the information for **property-based tests** for testing a student function with **bucketed scoring**. In particular, it specified
- The `input` type and `Output` structure for the student function being tested (specified as `submission : input -> Output.t`). The `OUTPUT` signature which `Output` is ascribed to is described in the [Helpers](#helpers) section
- A module `Bucket` which describes the bucket categories. It ascribes to `OUTPUT`, but this is only because the type `Bucket.t` requires print and equality operations. The `Bucket` module is used in the specification of `buckets : (Bucket.t * int) list`, which associates bucket categories with weights
- A list of `properties : property list` that will be tested. The `property` type is a record which contains the following:
    - `name : string` -- the name of the property
    - `bucket : Bucket.t` -- the bucket that this test is categorized by
    - `showInput` and `showOutput` (both of type `input -> string`) -- print functions for the expected input and output. `showOutput` is parameterized by type `input` since it will print the correct output for the given input, not the student's output
    - `gen : input QCheck.arbitrary` -- generator of random values of type `input`
    - `check : input -> Output.t -> bool` -- checks whether a given input/output pair satisfy the property being tested
    - `numTests : int` -- number of tests to be generated
    - `timeout` -- maximum time for running each test

#### `SCHEME`
---

The `SCHEME` signature describes how the tests are processed and scored. It is parameterized by an abstract `type 'a t`, which represents a consolidation of test results for scoring. The signature specifies the following 
- The `aggregate` function processes an `'a list` by applying the input predicate `'a -> 'b option` and collecting all of the `'b` values into a `'b t`
- The `score` function takes an `'a t` and converts it into its corresponding score, represented by the `zarith` type `Q.t` of rational numbers
- A `toString` function for the consolidated `'a t` type

To provide some intuition for what a `SCHEME` module can look like, consider the two implemented in [`BucketGrader.ml`](/util/BucketGrader.ml). First, for the `Make` functor, we implement the [following structure](/util/BucketGrader.ml#L7-L26) ascribing to `Scheme`:
```ocaml
struct
  type 'a t = 'a option

  let rec aggregate (f : 'a -> 'b option) : 'a list -> 'b t = function
    []      -> None
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
end : SCHEME
```
In this implementation, the consolidation type `'a t` is defined as an `'a option`, where `None` represents all tests passing and `Some v` representing the first failed test.

In the same file, for the `MakeList` functor, we implement the [following structure](/util/BucketGrader.ml#L36-L62) ascribing to `Scheme`:
```ocaml
struct
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

  let toString f (l, n) = (* omitted *)
end : SCHEME
```
In this implementation, the `'a t` type is a list of failed tests (of type `'a list`) alongside the count of passed test cases (of type `int`).

#### `Make` Functor
---

With both of these input structures specified, we may now define the body of the functor. Since the output ascribes to `GRADER`, we must define a `Rubric` structure and a `process` function.

For the `Rubric`, we define the type `t` as a `test Scheme.t list`, where the type `test` is a record defining the input, expected output, and student's actual output for a test. The output is defined as type `Input.Output.t Result.t`, where `Input.Output.t` is the output type as specified in the `BUCKET_INPUT` and `Result.t` represents the different possible outcomes from a student's function (see the [Helpers](#helpers) section). Scoring is performed by applying the `Scheme.score` function to each test case and then dividing the results by the associated bucket weight.

To implement `process`, we
1. Split up the property tests by their respective bucket (via [`partition`](/util/GeneralizedBucketGrader.ml#L98-L108))
2. Run the tests for each bucket and consolidate their result into a single `test Scheme.t` via `Scheme.aggregate`

The most important part of these steps is how the test is run, which is implemented [here]() using the following `QCheck` functionalities:
```ocaml
let checkProp submission (prop : Input.property) =
  let res = ref None in
  let test =
    QCheck.Test.make_cell
      ~name:prop.name
      ~count:prop.numTests
      prop.gen
      (fun input ->
         let r = Result.evaluate prop.timeout submission input in
           match r with
           | Result.Value v ->
             if prop.check input v then true
             else 
               (res := Some (Result.Value v, prop.showInput input, prop.showOutput input);
               false)
           | _ -> res := Some (r, prop.showInput input, prop.showOutput input); false)
  in
    (match QCheck2.TestResult.get_state (QCheck2.Test.check_cell test) with
      QCheck2.TestResult.Success -> None
    | _ -> !res)
```
The `checkProp` function will create the corresponding `QCheck` test cell for a given property. The `QCheck.Test.make_cell` function is given the name of the property being tested, the number of tests to generate, the generator for the input type, and then a function of type `input -> bool` which describes the property to be tested. In our case, we are interested in whether the output of the student submission has the expected property for a given input. To implement this, we use `Result.evaluate` to capture the output behavior of the student submission, and in the case that it returns a value, we use `prop.check` to see whether that return value has the right property.
Overall, this function will return `None` if all tests pass and `Some r` for `r : Output.t Result.t` if it fails, where the result `r` is the first failing output.

From here, all that remains is to run `checkProp` for each bucket and collect the results. 

## Additional Functors

As mentioned in the previous section, the remaining functors are all defined as calls to `GeneralizedBucketGrader.Make`. We provide a brief description of each of these files.

### [`BucketGrader.ml`](/util/BucketGrader.ml)

This file defines two functors: `Make` and `MakeList`. These two functors essentially have the same functionality as the original `GeneralizedBucketGrader`, but have specialized schemes defined via the `Scheme : SCHEME` input to the generalized bucket functor.

#### `BucketGrader.Make`
---
In this functor, the `Scheme` consolidates the scores by choosing the first failing test case (in the case of failure) or returning `None` (in the case of success). A successful test gets 1 point and a failing test gets 0. More details on how `Scheme` is defined for this functor can be found in the [`SCHEME`](#scheme) section.

#### `BucketGrader.MakeList`
---
In this functor, the `Scheme` consolidates the scores by collecting a list of failing tests and counting the number of passing tests. The score is then $\frac{n}{n + |\ell|}$, where `n` is the number of passing tests and $\ell$ is the list of failing tests. More details on how `Scheme` is defined for this functor can be found in the [`SCHEME`](#scheme) section.

### [`EquivGraderBucket.ml`](/util/EquivGraderBucket.ml)

This file defines two functors: `Make` and `MakeList`. These two functors are defined in terms of `BucketGrader.Make` and `BucketGrader.MakeList` (respectively), and specialize the bucket graders to specifically test the property of equivalence between the student submission and the refsol function (rather than some general property, which is what `BucketGrader` and `GeneralizedBucketGrader` are set up for).

The two functors take the following signature as input:
```ocaml
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
      timeout : int
    }
    val tests : test list
    val buckets : (Bucket.t * int) list

    val refsol     : input -> Output.t
    val submission : input -> Output.t
  end
```
The values specified in the signature are largely the same as `BUCKET_INPUT` except for two major changes:
- Instead of a `property` record type, there is now a `test` record type which simply records the bucket associated with the test, a generator for the input type, the number of tests to generate, and the timeout. See [here](/util/EquivGraderBucket.ml#L42-L56) to see how this type is mapped to the `property` type that `BucketGrader` expects
- The signature specifies a `refsol` to test the submission against

### [`EquivGrader.ml`](/util/EquivGrader.ml)

This is the functor most often used for day-to-day grading. As in the other files, we define a `Make` and `MakeList` functor, and these functors are declared in terms of `EquivGraderBucket.Make` and `EquivGraderBucket.MakeList` (respectively).
These functors, in addition to specifically testing equivalence between student submissions and reference solutions, also eliminate the bucket scoring and weight all tests equally (or in other words, put all tests into a single bucket). 

Similar to `EquivGraderBucket.ml`, we simplify the signature for the input to the `Make` and `MakeList` functors by defining the following signature:
```ocaml
module type EQUIV_INPUT =
  sig
    val description : string

    type input
    val show_input : input -> string
    module Output : OUTPUT

    type eqTest = {
      gen : input QCheck.arbitrary;
      numTests : int;
      timeout : int
    }
    val test : eqTest

    val refsol     : input -> Output.t
    val submission : input -> Output.t
  end
```

The only difference between this input signature and the one in [the previous section](#equivgraderbucketml) is that the `bucket` parameter from the test record type is removed, since there are no buckets.

### [`TrivialGrader.ml`](/util/TrivialGrader.ml)

As the name of the file suggests, the `TrivialGrader.Make` functor makes a grader that does not run any tests and immediately returns with a success. This grader is used when defining the Optimistic Checkscript, where we only need to typecheck the files for them to "pass" the autograder.

## Helpers

The `util` directory also contains the following subdirectories/helper modules, which are used in the implementation of the main autograder functors:

- [`output/`](/util/output/) 
    - Contains the [`OutputSig.ml`](/util/output/OutputSig.ml) signature, which specifies an equality and print function for the output type of whatever function we're testing
    - The signature is set up to work with `ppx_deriving` whenever possible (see the [`SumGrader`](/tests/SumGrader.ml) for reference), otherwise you can just implement the specified operations directly when using a grading functor
- [`prod/`](/util/prod/)
    - Contains the grader combinators, allowing you to combine multiple graders into a single one and assign weights to each component
    - Supports the combination of up to 9 graders (via separate combinators)
- [`result/`](/util/result/)
    - Contains the `Result` module, which provides support for valuable, exceptional, and timeout results from running a student's function
- [`FormatUtil.ml`](/util/FormatUtil.ml)
    - A module for string formatting and some other helper functions used throughout the framework, notably an implementation of function composition as the symbol `(>>)`
- [`Preamble.ml`](/util/TrivialGrader.ml)
  - A functor that allows you to append some additional preamble text to the description of a grader