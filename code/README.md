
Install OCaml

```
brew install opam
# environment setup
opam init
eval $(opam env)
# install given version of the compiler
opam switch create 4.12.0
eval $(opam env)
# check you got what you want
which ocaml
ocaml -version
```

Install OASIS (https://github.com/ocaml/oasis/blob/master/doc/QUICKSTART.md)
```
opam install oasis
opam install core
```

compile
```
make
```

run
```
./main.native
```


