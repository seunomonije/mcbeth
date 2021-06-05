# Project Documentation

## Tables of Contents

1. [Installation](#installation)
   1. [Quick Start](#quick-start)
   2. [Opam](#opam)
   3. [Dune](#dune)
   4. [Lacaml](#lacaml)
2. [Project Structure](#project-structure)
   1. [File Organization](#file-organization)
   2. [Libraries](#libraries)
      1. [Lacaml](#lacaml-1)
      2. [Complexenv (Cenv)](#complexenv-cenv)
   3. [Running Tests](#running-tests)
3. [Programming in MCL](#programming-in-mcl)
   1. [Writing Programs](#writing-programs)
   2. [Compiling Programs](#compiling-programs)

## Installation

### Quick Start

The following commands worked to install all needed dependencies on Ubuntu 20.04:

```
sudo apt-get install opam
opam init
eval $(opam config env)
opam install dune
opam install dune-configurator
sudo apt-get install libblas-dev liblapack-dev
```

More detail is available in the following sections.

### Opam

Opam is a package manager for OCaml projects -- the language MCL is primarily written in. Opam allows for the quick and easy installation of required packages; specifically, we use it to install and manage the Dune build system, discussed below.

Install Opam via `apt` by running `sudo apt install opam`. Then, run `opam init` and follow the instructions. Typically, it will instruct you to run `eval $(opam config env)` or something similar.

For more installation options and instructions, visit https://opam.ocaml.org/doc/Install.html.

Learn more about Opam at https://opam.ocaml.org/.

### Dune

We use Dune to build and test our project. Mentioned briefly earlier, Dune is a build system used to manage OCaml projects. It helps compile, run, and test OCaml code.

Install Dune by running `opam install dune`.

For more installation options and to learn more about Dune, visit https://github.com/ocaml/dune.

### Lacaml

Lacaml is an OCaml library from linear algebra. It interfaces with the [BLAS](http://www.netlib.org/blas/) and [LAPACK](http://www.netlib.org/lapack/) linear algebra libraries, which are widely used for fast performing fast linear algebra operations.

The Lacaml library is included as the submodule "lacaml" located in the [lib](/mclang/lib) directory but still requires some dependencies to be installed.

First, the Lacaml library requires the [dune-configurator library](https://opam.ocaml.org/packages/dune-configurator/). Install it by running `opam install dune-configurator`.

Second, the Lacaml library requires the BLAS and LAPACK libraries. Install them both by running `sudo apt-get install libblas-dev liblapack-dev`.

Learn more about Lacaml at https://mmottl.github.io/lacaml/ and https://github.com/mmottl/lacaml.

## Project Structure

As mentioned in the above section, we used Dune to help build and test our project.

### File Organization

The project is currently split into three main folders: [backend](/mclang/backend), [lib](/mclang/lib), and [tests](/mclang/tests).

The [backend](/mclang/backend) directory contains the main files for creating, processing, and running MCL programs. Specifically, [types.mli](/mclang/backend/types.mli) contains the data type definitions used to write MCL programs, [run.ml](/mclang/backend/run.ml) contains the code used to simulate MCL programs, and [presets.ml](/mclang/backend/presets.ml) contains functions which can be used to help write common quantun algorithms. More details on the contents of these files are explained in the [Writing Programs](#writing-programs) section below.

The [lib](/mclang/lib) directory contains the libraries that MCL programs require in order to run. Details about these libraries are discussed in the [Libraries](#libraries) section below.

The [tests](/mclang/tests) directory contains various tests used during development. How to run these tests and create new tests are explained in the [Running Tests](#running-tests) section below.

### Libraries

#### Lacaml

[Lacaml](https://github.com/mmottl/lacaml) provides the linear algebra operations needed to calculate and store quantum state information. More information about what the Lacaml is is provided [under the Installation section](#lacaml).

We primarily rely on the [Lacaml.Z](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/index.html) module for calculations involving double precision complex numbers. We also use the [Lacaml.Io](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Io/index.html) module for pretty printing matrices and vectors. Links to these and other helpful modules are listed below.

##### Helpful Module Links:

- [Lacaml](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/index.html): the main module
- [Lacaml.Io](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Io/index.html): functions for representing matrices and vectors as strings
- [Lacaml.Z](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/index.html): functions using double precision complex numbers
- [Lacaml.Z.Vec](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/Vec/index.html): creating and performing operations on vectors
- [Lacam.Z.Mat](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/Mat/index.html): creating and performing operations on matrices

The Lacaml library is refered to as `lacaml` in dune files -- [dune file link](https://github.com/mmottl/lacaml/blob/master/src/dune).

#### Complexenv (Cenv)

[Complexenv](/mclang/lib/complexenv), or Cenv for short, is a custom-made library to make the use of complex numbers easier in OCaml. Cenv contains new helpful functions and redefines arithmatic operators to use complex numbers instead of integers. It uses the complex numbers type `Complex.t` defined the OCaml standard library [Complex module](https://ocaml.org/api/Complex.html); this same type is [used in Lacaml](http://mmottl.github.io/lacaml/api/lacaml/Lacaml/Z/index.html#type-num_type).

##### Functions

- `val c : float -> float -> Complex.t`<br>
  The function `c re im` creates a new complex number given two floats, `re` and `im`. `re` is the real part of the complex number and `im` is the imaginary part.<br>
  Note: when passing negative numerals as arguments, the numeral must be wrapped in parenthesis and use `-.` to negate the value. E.g., to create the complex number "-1.3+1.0i" call `c (-.1.3) 1.0`.
- `val cstr : Complex.t -> string`<br>
  The function `cstr n` converts the complex number `n` into a string. For example, say `n` was a complex number with the real part equal to 3.0 and the imaginary party equal to 2.1; `cstr n` would produce the string, "3.00+2.1i".

##### Redefined Operators

### Running Tests
- Addition (`x + y`)
- Subtraction (`x - y`)
- Unary Negation (`-x`)
- Multiplication (`x * y`)
- Division (`x / y`)

The Cenv library is refered to as `cenv` in dune files -- [dune file link](/mclang/lib/complexenv/dune).

##### New Operators

- Exponentiation (`x ** y`; "x to the power of y")

## Programming in MCL

#### Writing Programs

[run.mli](/mclang/backend/run.mli) lists the functions contained in [run.ml](/mclang/backend/run.ml) which are available in the `Backend.Run` module.

#### Compiling Programs

We can build with `dune build [file_name.exe]` and then run with `dune exec file_name.exe`.
