(lang dune 2.0)
(generate_opam_files true)
(version 0.1)
(maintainers "Simon Cruanes")
(license MIT)
(source (github c-cube/ocaml-avro))
(authors "Simon Cruanes")
(bug_reports "https://github.com/c-cube/ocaml-avro/issues/")
(homepage "https://github.com/c-cube/ocaml-avro")
(name avro)


(package
  (name avro)
  (synopsis "Runtime library for encoding/decoding Avro")
  (depends
    (dune (>= 2.0))
    (odoc :with-doc)
    (avro-compiler :with-test)
    (ocaml (>= 4.08.0))
    camlzip))

(package
  (name avro-compiler)
  (synopsis "Schema compiler for Avro")
  (depends
    (dune (>= 2.0))
    (ocaml (>= 4.08.0))
    (odoc :with-doc)
    (yojson (>= 1.6))))
