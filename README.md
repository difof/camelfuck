# CamelFuck - Brainfuck interpreter

My day 2 attempt of learning the amazing language of OCaml was making a Brainfuck interpreter.
I may add some optimizations later on for no reason.

All of the implementation is at [bin/main.ml](./bin/main.ml)

# Usage

Make sure you have OCaml, opam and dune installed, then do these:

```sh
opam install . --deps-only
dune exec camelfuck bfprograms/helloworld.bf
```

There are a bunch of programs in the [bfprograms](./bfprograms/) directory which I copy pasted from internet.

# License
MIT