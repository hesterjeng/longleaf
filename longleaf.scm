(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix gexp)
             (guix build-system dune)
             ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages ocaml)
             (gnu packages version-control)
             (gnu packages pkg-config)
             (gnu packages compression)
             (gnu packages tls)
             (gnu packages curl))

;; Load our local package definitions
(load "packages.scm")

;; Full Longleaf package with all dependencies
(package
  (name "longleaf")
  (version "1.0.2")
  (source (local-file "." "longleaf-checkout"
                      #:recursive? #t
                      #:select? (git-predicate ".")))
  (build-system dune-build-system)
  (arguments
   `(#:package "longleaf-lib,longleaf-strategies"
     #:test-target "."))
  (native-inputs
   (list ocaml-alcotest ocaml-odoc))
  (propagated-inputs
   (list ocaml
         dune
         ocaml-ptime
         ocaml-ppx-yojson-conv-lib
         ocaml-ppx-deriving
         ocaml-ppx-variants-conv
         ocaml-ppx-fields-conv
         ocaml-cmdliner
         ocaml-landmarks      ; from packages.scm
         ocaml-iter           ; from packages.scm
         ocaml-containers     ; from packages.scm
         ocaml-graph
         ocaml5.0-eio-main
         ocaml-piaf           ; from packages.scm
         ;; tacaml - custom package, handle separately
         ocaml-fileutils
         ocamlformat
         ocaml-yojson
         ;; System dependencies
         pkg-config
         zlib
         openssl
         curl
         git))
  (home-page "https://github.com/hesterjeng/longleaf")
  (synopsis "Algorithmic trading platform written in OCaml")
  (description
   "Longleaf is an algorithmic trading platform that supports live trading,
paper trading, and backtesting with multiple brokerages and market data sources.
The platform uses a functional, modular architecture with strategies implemented
as functors for maximum code reuse and type safety.

Note: tacaml (custom TA-Lib bindings) must be installed separately.")
  (license license:gpl3+))