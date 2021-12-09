(lang dune 2.0)
(generate_opam_files true)


(package
  (name avro)
  (synopsis "Runtime library for encoding/decoding Avro")
  (depends
    (ocaml (>= 4.08.0))))
