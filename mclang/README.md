# Project Documentation

## Table of Contents

1. [Installation](#installation)
   1. [Quick Start](#quick-start)
   2. [Opam](#opam)
   3. [Dune](#dune)
   4. [Lacaml](#lacaml)
   5. [Troubleshooting](#troubleshooting)
2. [Project Structure](#project-structure)
   1. [File Organization](#file-organization)
   2. [Libraries](#libraries)
      1. [Lacaml](#lacaml-1)
      1. [Lacamlext](#lacamlext)
      1. [Complexenv (Cenv)](#complexenv-cenv)
      1. [Created Dune Libraries](#created-dune-libraries)
   3. [Test Cases](#test-cases)
      1. [Running Tests](#running-tests)
      2. [Creating Tests](#creating-tests)
      3. [Available Test Cases](#available-test-cases)
3. [Programming in MCL](#programming-in-mcl)
   1. [MCL Libraries](#mcl-libraries)
      1. [Backend](#backend)
   2. [Writing Programs](#writing-programs)
      1. [Overview](#overview)
      2. [Preparations](#preparations)
      3. [Commands](#commands)
      4. [Program Constraints](#program-constraints)
   3. [Compiling Programs](#compiling-programs)

## Installation

### Quick Start

#### Linux

The following commands will install all needed dependencies on Ubuntu 20.04:

```
sudo apt-get install opam
opam init
eval $(opam config env)
opam install dune
opam install dune-configurator
sudo apt-get install libblas-dev liblapack-dev
```

#### MacOS

It's recommended to use package manager [Homebrew](https://brew.sh/) to download dependencies that aren't reliant on Opam.
Translating the quick start above, this looks like:

```
brew install opam
opam init
eval $(opam config env)
opam install dune
opam install dune-configurator
brew install openblas lapack
```

Note that MacOS comes pre installed with the [vecLib framework](https://developer.apple.com/documentation/accelerate/veclib) which
already contains both BLAS and LAPACK. You can point this project to those pre-existing installations, or download a fresh
set.

When cloning the repository, be sure to pull in all submodules with it by running `git submodule update --init --recursive`.

More detail is available in the following sections.

### Core dependencies

#### Opam

Opam is a package manager for OCaml projects -- the language MCL is primarily written in. Opam allows for the quick and easy installation of required packages; specifically, we use it to install and manage the Dune build system, discussed below.

Install Opam via `apt` by running `sudo apt install opam`. Then, run `opam init` and follow the instructions. Typically, it will instruct you to run `eval $(opam config env)` or something similar.

For more installation options and instructions, visit https://opam.ocaml.org/doc/Install.html.

Learn more about Opam at https://opam.ocaml.org/.

#### Dune

We use Dune to build and test our project. Mentioned briefly earlier, Dune is a build system used to manage OCaml projects. It helps compile, run, and test OCaml code.

Install Dune by running `opam install dune`.

For more installation options and to learn more about Dune, visit https://github.com/ocaml/dune.

#### Lacaml

Lacaml is an OCaml library from linear algebra. It interfaces with the [BLAS](http://www.netlib.org/blas/) and [LAPACK](http://www.netlib.org/lapack/) linear algebra libraries, which are widely used for fast performing fast linear algebra operations.

The Lacaml library is included as the submodule "lacaml" located in the [lib](/mclang/lib) directory but still requires some dependencies to be installed.

First, the Lacaml library requires the [dune-configurator library](https://opam.ocaml.org/packages/dune-configurator/). Install it by running `opam install dune-configurator`.

Second, the Lacaml library requires the BLAS and LAPACK libraries. Install them both by running `sudo apt-get install libblas-dev liblapack-dev`.

Learn more about Lacaml at https://mmottl.github.io/lacaml/ and https://github.com/mmottl/lacaml.

#### Pyml, pythonlib

Pyml - Bindings for Python 2 and 3. `opam install pyml`.

pythonlib - Wrapper to simplify the connection between Python and Ocaml

### Troubleshooting

TODO

## Project Structure

As mentioned in the above section, we used Dune to help build and test our project.

### File Organization

The project is currently split into three main folders: [backend](/mclang/backend), [lib](/mclang/lib), and [tests](/mclang/tests).

The [backend](/mclang/backend) directory contains the files in MCL's Backend library. More details on this library are discussed in the [Backend section of MCL Libraries](#backend).

The [lib](/mclang/lib) directory contains the external libraries that MCL programs require in order to run. Details about these libraries are discussed in the [Libraries](#libraries) section below.

The [tests](/mclang/tests) directory contains various tests used during development. How to run these tests and create new tests are explained in the [Running Tests](#running-tests) section below.

### Libraries

#### Lacaml

[Lacaml](https://github.com/mmottl/lacaml) provides the linear algebra operations needed to calculate and store quantum state information. More information about what the Lacaml external library is is provided [under the Installation section](#lacaml).

We primarily rely on the [Lacaml.Z](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/index.html) module for calculations involving double precision complex numbers. We also use the [Lacaml.Io](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Io/index.html) module for pretty printing matrices and vectors. Links to these and other helpful modules are listed below.

##### Helpful Module Links:

- [Lacaml](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/index.html): the main module
- [Lacaml.Io](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Io/index.html): functions for representing matrices and vectors as strings
- [Lacaml.Z](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/index.html): functions using double precision complex numbers
- [Lacaml.Z.Vec](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/Vec/index.html): creating and performing operations on vectors
- [Lacam.Z.Mat](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/Mat/index.html): creating and performing operations on matrices

The Lacaml library is refered to as `lacaml` in dune files -- [dune file link](https://github.com/mmottl/lacaml/blob/master/src/dune).

#### Lacamlext

[Lacamlext](/mclang/lib/lacamlext) is a custom-made library which extends the Lacaml library to include more functions. To use the the library, include `open Lacamlext;;` at the very beginning of any file which using the Lacaml module.

##### Functions

- Print<br>
  `Lacaml.Z.Vec` and `Lacaml.Z.Mat` are extended to include functions which pretty print a vector or matrix to standard output.<br>
  - `val print : Vec.t -> unit`
  - `val print : Mat.t -> unit`
- Scalar Multiplication<br>
  `Lacaml.Z.Vec` and `Lacaml.Z.Mat` are extended to include scalar multiplication functions which multiply each element of a vector or matrix by a complex scalar:<br>
  - `val scal_mul : Complex.t -> Vec.t -> Vec.t`
  - `val scal_mul : Complex.t -> Mat.t -> Mat.t`
- Tensor Product<br>
  `Lacaml.Z.Vec` and `Lacaml.Z.Mat` are extended to include tensor product functions:<br>

  - `val tensor_prod : Vec.t -> Vec.t -> Vec.t`
  - `val tensor_prod : Mat.t -> Mat.t -> Mat.t`
  - `val tensor_prod_list : Vec.t list -> Vec.t`
  - `val tensor_prod_list : Mat.t list -> Mat.t`
  - `val tensor_prod_arr : Vec.t array -> Vec.t`
  - `val tensor_prod_arr : Mat.t array -> Mat.t`

  `tensor_prod` calculates the tensor product between two vectors or two matrices; i.e., x<sub>1</sub> &otimes; x<sub>2</sub>.

  `tensor_prod_list` and `tensor_prod_arr` produces the tensor product of multiple vectors or matrices, evaluating the list or array from left to right; i.e., x<sub>1</sub> &otimes; x<sub>2</sub> &otimes; &hellip; &otimes; x<sub>n</sub>.

#### Complexenv (Cenv)

[Complexenv](/mclang/lib/complexenv), or Cenv for short, is a custom-made external library to make the use of complex numbers easier in OCaml. Cenv contains new helpful functions and redefines arithmatic operators to use complex numbers instead of integers. It uses the complex numbers type `Complex.t` defined the OCaml standard library [Complex module](https://ocaml.org/api/Complex.html); this same type is [used in Lacaml](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/index.html#type-num_type).

##### Functions

- `val c : float -> float -> Complex.t`<br>
  The function `c re im` creates a new complex number given two floats, `re` and `im`. `re` is the real part of the complex number and `im` is the imaginary part.<br>
  Note: when passing negative numerals as arguments, the numeral must be wrapped in parenthesis and use `-.` to negate the value. E.g., to create the complex number "-1.3+1.0i" call `c (-.1.3) 1.0`.
- `val cstr : Complex.t -> string`<br>
  The function `cstr n` converts the complex number `n` into a string. For example, say `n` was a complex number with the real part equal to 3.0 and the imaginary party equal to 2.1; `cstr n` would produce the string, "3.00+2.1i".

##### Redefined Operators

- Addition (`x + y`)
- Subtraction (`x - y`)
- Unary Negation (`-x`)
- Multiplication (`x * y`)
- Division (`x / y`)

##### New Operators

- Exponentiation (`x ** y`; "x to the power of y")

The Cenv library is refered to as `cenv` in dune files -- [dune file link](/mclang/lib/complexenv/dune).

#### Created Dune Libraries

To ease development, two libraries are currently defined in the [main dune file](/mclang/dune) for the project. These libraries simply group other modules and libraries together.

##### mcl

The `mcl` library groups all of the parts of MCL into one dune library so all of MCL can be included in a build by just adding `mcl` to a dune stanza's `libraries` field. Currently, `mcl` just includes [the backend](#backend). MCL libraries, i.e., "the parts of MCL," are further discussed in the [MCL Libraries](#mcl-libraries) section.

##### linalg

The `linalg` library groups the `lacaml` and `cenv` external libraries into one dune library so all linear algebra functions can be included in a build by just adding `linalg` to a dune stanza's `libraries` field.

### Test Cases

We also use Dune to manage test cases via its `runtest` command and `tests` stanza. All tests are contained in subdirectories of the [tests](/mclang/tests) directory.

#### Running Tests

To run all tests, run `dune runtest tests --force`.

To run only a subset of tests, run `dune runtest tests/subdir --force` where `subdir` is the subdirectory containing the desired subset of tests.

##### The `--force` Flag

Adding the `--force` flag to the end of the runtest command forces all of the specified test cases run. Without the flag, only tests which have been modified since the last runtest call will run.

Not using the force flag is useful when creating or developing tests. Because only modified tests will run, no already developed tests will run and, therefore, the output will only contain the current tests being worked on.

#### Creating Tests

Tests are defined by adding the module name to the dune file in the tests' subdirectory.

Each dune file looks something like this:

```
;; Example Tests

(tests
  (names example1 test1337 prog7)
  (libraries mcl linalg)
)
```

The `names` field lists each test that should be included. In this example, the tests "example1", "test1337", and "prog7" are included. Each test name should have a corresponding `.ml` file in the same directory; for example, "example1" should have a corresponding "example1.ml" file.

The `libraries` field lists all libraries that running these tests depends on. In this example, these tests need the "mcl" and "linalg" libraries.

Say we wish to add the test "new_test" to some set of tests.

If "new_test" is being added to an already created subset of tests, then paste the "new_test.ml" file in the desired subdirectory of [tests](/mclang/tests) and then add "new_test" to that subdirectory's dune file's `names` field. For example, the `names` field of the above example file would now be:

```
  (names example1 test1337 prog7 new_test)
```

If "new_test" is the first test in a new subset of tests, then create a new subdirectory in [tests](/mclang/tests) and create a file based on the example file above. For the `names` field, just include "new_test":

```
  (names new_test)
```

#### Available Test Cases

The following test case subsets are currently in the [tests](/mclang/tests) directory:

- [la](/mclang/tests/la): linear algebra test cases
- [wf](/mclang/tests/wf): test cases for the `well_formed` function in [backend/run.ml](/mclang/backend/run.ml)
- [one](/mclang/tests/one): a single test case, [one.ml](/mclang/tests/one/one.ml), used for running quick tests

TODO: add test cases within each subset

## Programming in MCL

### MCL Libraries

#### Backend

The MCL Backend library is responsible for processing and running MCL programs; it also contains the data types used when writing MCL programs. The [backend](/mclang/backend) directory contains all of the files used in the library. Each file defines a module within the Backend library; each module contains functions available for use when creating and testing MCL programs. Each `.ml` file has a corresponding `.mli` file which lists the functions in the `.ml` file available for use outside of the `.ml` file.

##### Files List

- [types.mli](/mclang/backend/types.mli): contains the data type definitions used to write MCL programs
- [run.ml](/mclang/backend/run.ml): contains the functions used to process MCL programs
- [presets.ml](/mclang/backend/presets.ml): contains functions which can be used to help write common quantun algorithms

##### Modules

###### Types

- `type qubit = int`<br>
  Qubits are currently represented as integers.
- `type prep = Init of qubit * float | Init0 of qubit | Init1 of qubit | InitPlus of qubit | InitMinus of qubit | InitNonInput of qubit list`<br>
  Type `prep` defines the instructions used to prepare qubits. <br> Details on these instructions are provided in the [Writing Programs](#writing-programs) section below.
- `type cmd = Entangle of qubit * qubit | Measure of qubit * float * qubit list * qubit list | XCorrect of qubit * qubit list | ZCorrect of qubit * qubit list`<br>
  Type `cmd` defines the instructions used to perform specific actions an the qubits. <br> Details on these instructions are provided in the [Writing Programs](#writing-programs) section below.
- `type prog = prep list * cmd list`<br>
  Type `prog` defines what an entire program consists of. A program is defined as a pair of lists of instructions: one of prepare instructions and one of command instructions.

###### Run

- `val print_prog : prog -> unit`<br>
  This function takes a program as input and prints to standard output the a more readable version of it.
- `val well_formed : prog -> int`<br>
  This function ensures that the program is valid. A program is valid if it does not violates the constraints described in the [Program Constraints](#program-constraints) section below. If valid, then the function returns the number of qubits used; if not valid, then the function returns 0.
- `val eval : prog -> bool list`<br>
  This function runs a program by executing each instruction while keeping track of the resulting qubit states. It returns a list of booleans corresponding to the final measured states of each qubit; `true` for 1 and `false` for 0.

###### Presets

TODO

### Writing Programs

#### Overview

TODO

#### Preparations

- `InitNonInput([q1; q2; ...])`: Sets all qubits which are not used for inputto the state `|+>`.

All other instructions act on input qubits to set them to some input state as follows:

- `Init(q, f)`: Sets the state of qubit `q` to the state `f`.
- `Init0(q)`: Sets `q` to `|0>`.
- `Init1(q)`: Sets `q` to `|1>`.
- `InitPlus(q)`: Sets `q` to `|+>`.
- `InitMinus(q)`: Sets `q` to `|->`.

If a qubit is used for both input and output, **do not** include it in `InitNonInput`.

#### Commands

- `Entangle(q1, q2)`: Entangles qubits `q1` and `q2`.
- `Measure(q, angle, signals1, signals2)`: Measures qubit `q` at an angle based on the value of `angle`and on the outcomes of the qubits specified in `signals1` and `signals2`.
- `XCorrect(q, signals)`: Performs Pauli X correction based on the outcomes of the qubits specified in `signals`.
- `ZCorrect(q, signals)`: Performs Pauli Z correction based on the outcomes of the qubits specified in `signals`.

TODO: explain signals

#### Program Constraints

A program is valid, i.e., well formed, if it does not violate the following constraints:

##### General Constraints:

- (D0) No command depends on an outcome not yet measured. In other words, only qubits which have been measured can be included in a "signals" list.
- (D1) No command acts on a qubit already measured.
- (D2) No command acts on a qubit not yet prepared, unless it is an input qubit. In other words, all qubits must be in some state.
- (D3) A qubit `i` is measured _if and only if_ `i` is not an output. Equivalently, a qubit `i` is an output _iff_ `i` is not measured.

##### Implementation-based Constraints:

- (D4) The "qubit integers" must start at 0 and increase without skipping an integer. "qubit integers" refers to the integer each qubit is defined as, since qubits are defined as integers currently.

### Compiling Programs

To compile a program as an executable, put the written program in [main.ml](/mclang/main.ml) and run `dune build main.exe`. To then run the compiled program, run `dune exec ./main.exe`.
