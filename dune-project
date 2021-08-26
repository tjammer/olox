(lang dune 2.9)
(name olox)
(cram enable)

(generate_opam_files true)

(package
 (name olox)
 (version 1.0.0)
 (description "OCaml implementation of jlox")
 (synopsis "OCaml implementation of jlox")
 (depends
  dune
  (containers (>= 3.5))
  (mtime (>= 1.2.0))
  (ppx_deriving (>= 5.2.1))))
