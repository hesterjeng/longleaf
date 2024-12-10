(lang dune 3.9)

(name longleaf)

(generate_opam_files true)

; (source
;  (github john/longleaf))

(authors "")

(maintainers "")

(package
 (name longleaf)
 (synopsis "longleaf")
 (license "none")
 (depends
  dune
  (ocaml
   (>= "5.2"))
  (containers
   (>= "3.4"))
  (containers-data
   (>= "3.4"))
  (iter
   (>= "1.0"))
  zarith
  ezcurl-lwt
  logs
  ptime
  tsort
  uuidm
  ptime
  owl
  (yojson
   (>= "1.6"))
  ppx_yojson_conv
  ppx_deriving
  dream
  ppx_variants_conv
  ppx_fields_conv
  cmdliner
  ocamlgraph
  alcotest
  lwt
  eio_main
  piaf
  fileutils
  ocamlformat
  alcotest))
