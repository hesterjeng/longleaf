(lang dune 3.17)

(name longleaf)

(generate_opam_files true)

(authors "John Hester")

(maintainers "John Hester")

(package
 (name longleaf-lib)
 (synopsis "Longleaf")
 (license "GPL-3.0-or-later")
 (homepage "https://github.com/hesterjeng/longleaf")
 (bug_reports "Github Issue")
 (depends
  dune
  ocaml
  ptime
  ppx_yojson_conv
  ppx_deriving
  ppx_variants_conv
  ppx_fields_conv
  cmdliner
  landmarks
  iter
  owl
  containers-data
  ocamlgraph
  eio_main
  piaf
  fileutils
  ocamlformat))

(package
 (name longleaf-strategies)
 (synopsis "Longleaf Strategies")
 (license "GPL-3.0-or-later")
 (homepage "https://github.com/hesterjeng/longleaf")
 (bug_reports "Github Issue")
 (depends
  dune
  ocaml
  ptime
  ppx_yojson_conv
  ppx_deriving
  ppx_variants_conv
  ppx_fields_conv
  cmdliner
  landmarks
  iter
  owl
  containers-data
  ocamlgraph
  eio_main
  piaf
  fileutils
  ocamlformat))

(package
 (name longleaf-strategies)
 (synopsis "Longleaf Strategies")
 (license "GPL-3.0-or-later")
 (homepage "https://github.com/hesterjeng/longleaf")
 (bug_reports "Github Issue")
 (depends
  dune
  ocaml
  ptime
  ppx_yojson_conv
  ppx_deriving
  dream
  ppx_variants_conv
  ppx_fields_conv
  cmdliner
  landmarks
  iter
  owl
  containers-data
  ocamlgraph
  eio_main
  piaf
  fileutils
  ocamlformat))
