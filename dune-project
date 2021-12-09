(lang dune 2.0)
(generate_opam_files true)


(package
  (name avro)
  (synopsis "Runtime library for encoding/decoding Avro")
  (depends
    (dune (>= 2.0))
    (odoc :with-doc)
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
