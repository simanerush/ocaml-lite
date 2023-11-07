(lang dune 3.8)

(name ocaml_lite)

(generate_opam_files true)

(authors "Sima Nerush")

(license BSD3)

(package
 (name ocaml_lite)
 (synopsis "OCaml-lite language.")
 (depends ocaml dune menhir nice_parser ppx_deriving))

(using menhir 2.1)