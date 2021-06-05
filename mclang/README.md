# MCL Documentation

## Tables of Contents

1. [Installation](#installation)
   1. [Quick Start](#quick-start)
   2. [Opam](#opam)
   3. [Dune](#dune)
   4. [Lacaml](#lacaml)
2. [File Organization](#file-organization)
3. [Development and Testing](#development-and-testing)

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

The Lacaml library is included as a submodule in [/lib/lacaml](/mclang/lib/lacaml) but still requires some dependencies to be installed.

First, the Lacaml library requires the [dune-configurator library](https://opam.ocaml.org/packages/dune-configurator/); install it by running `opam install dune-configurator`.

Second, the Lacaml library requires the BLAS and LAPACK libraries. Install them both by running `sudo apt-get install libblas-dev liblapack-dev`.

Learn more about Lacaml at https://mmottl.github.io/lacaml/ and https://github.com/mmottl/lacaml.

## File Organization

## Development and Testing

We can build with `dune build [file_name.exe]` and then run with `dune exec file_name.exe`.
