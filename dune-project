(lang dune 3.17)

(name longleaf)

(generate_opam_files true)

(authors "John Hester")

(maintainers "John Hester")

(package
 (name longleaf)
 (synopsis "longleaf")
 (license "GPLv3")
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
  ezcurl-lwt
  ptime
  uuidm
  ptime
  (yojson
   (>= "1.6"))
  ppx_yojson_conv
  ppx_deriving
  dream
  ppx_variants_conv
  ppx_fields_conv
  cmdliner
  ocamlgraph
  eio_main
  piaf
  fileutils
  ocamlformat))
