# Python interface for MCL

### Installation

You can install and use the package like so:

```
TODO: Insert pip install command.
```

## Getting Started

TODO: Quick start, examples, Public Colab/Jupyter notebooks.

## Linking Python and OCaml

We utilize Jane street's libraries pyml and pythonlib to help us. If you're
interested in learning more about how to do this yourself, see [this](https://blog.janestreet.com/using-python-and-ocaml-in-the-same-jupyter-notebook/) article.

For JSON, we use [yojson](https://github.com/ocaml-community/yojson).

Helpful commands to build everything
Build -> `dune build .`.
Wrote a script that automatically copies the output binary to the package for use
in the interface.