(lang dune 3.19)

(name camelfuck)

(generate_opam_files true)

(source
 (github difof/camelfuck))

(license MIT)

(package
 (name camelfuck)
 (synopsis "Brainfuck interpreter")
 (description "Brainfuck interpreter in OCaml.")
 (depends ocaml)
 (tags
  (ocaml brainfuck)))

(package
 (name tape))
