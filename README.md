# OCaml Autograding Utilities

This repository provides a framework for autograding in OCaml 
This is a reimplementation of the [ProjectSavannah/autograding](https://github.com/ProjectSavanna/autograding) utilities, forked from the [gradescope_ocaml](https://github.com/qcfu-bu/gradescope_ocaml) repository.

## Overview

This repository provides an adaptation of the [ProjectSavannah/autograding](https://github.com/ProjectSavanna/autograding) framework with additional QCheck integration for automatic test case generation.
The codebase, in addition to implementing the autograding functionalities, provides a demo for grading the file [`sum.ml`](/impl/sum.ml). It has the following structure:
- [`bin/`](/bin/)
  - [`main.ml`](/bin/main.ml) - the primary grading mechanism, which runs the designated autograders for each task in a problem according to [`GraderMap.ml`](/tests/GraderMap.ml)
- [`impl/`](/impl/)
  - Where the student implementation is stored during grading
- [`refsol/`](/refsol/)
  - Where the reference solution is stored during grading
- [`tests/`](/tests/)
  - [`GraderMap.ml`](/tests/GraderMap.ml) - a mapping of tasks to their respective autograder for each problem in an assignment
  - Contains the autograders for each task (for all problems on an assignment)
- [`util/`](/util/)
  - Autograding utility functions

To run the demo, you should run `setup.sh` followed by `run-autograder`.

For a detailed description of the grading utilities, see [here](util/README.md).

## Acknowledgments

The code in this repository is based on the following two projects:

- [ProjectSavannah/autograding](https://github.com/ProjectSavanna/autograding), as this project is effectively a one-to-one translation of the SML autograding utilities defined there,
- [gradescope_ocaml](https://github.com/qcfu-bu/gradescope_ocaml), which provided guidance for how to integrate OCaml grading frameworks with Gradescope