(lang dune 3.10)

(name ambience)

(generate_opam_files true)

(source
 (github ethanshenley/ambience))

(authors "Ambient Commerce Protocol Team")

(maintainers "ethan.henley@sylow.io")

(license MIT)

(documentation https://github.com/ethanshenley/ambience)

(package
 (name ambience)
 (synopsis "Ambient Commerce Protocol - Distributed agent economic interactions")
 (description "A distributed system for autonomous agent economic interactions featuring persistent intents, continuous matching, and cryptographic settlement")
 (depends
  ocaml
  dune
  (yojson (>= 2.0.0))
  (ppx_deriving_yojson (>= 3.7.0))
  (lwt (>= 5.6.0))
  (cohttp-lwt-unix (>= 5.0.0))
  (alcotest :with-test)
  (odoc :with-doc))
 (tags
  (distributed protocol commerce agents)))