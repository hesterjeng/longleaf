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
             (gnu packages curl)
             (gnu packages node)
             (gnu packages maths)
             (gnu packages finance)
             (gnu packages libffi))

;; Load updated ctypes package
(load "ocaml-ctypes.scm")

;; Define tacaml package
(define-public tacaml
  (package
   (name "tacaml")
   (version "0.1.0")
   (source (local-file "../tacaml" "tacaml-checkout"
                       #:recursive? #t
                       #:select? (git-predicate "../tacaml")))
   (build-system dune-build-system)
   (arguments
    `(#:test-target "."))
   (native-inputs
    (list ocaml-odoc))
   (propagated-inputs
    (list ocaml
          dune
          ocaml-ctypes-latest
          ocaml-ppx-deriving
          ocaml-ppx-hash
          ta-lib
          pkg-config))
   (home-page "https://github.com/hesterjeng/tacaml")
   (synopsis "OCaml bindings for TA-Lib technical analysis library")
   (description
    "tacaml provides OCaml bindings to the TA-Lib (Technical Analysis Library).
This project offers both raw C bindings and higher-level, type-safe wrappers
for over 160 technical analysis functions commonly used in financial markets.
Features include comprehensive bindings, type safety with GADTs, efficient
data handling with Bigarray integration, modular design, and robust error
handling with Result types.")
   (license license:gpl3+)))

;; Load React frontend package
(load "react/longleaf-frontend.scm")

;; Load QuantStats server package
(load "tearsheets/longleaf-quantstats.scm")

;; Full Longleaf package with all dependencies
(package
 (name "longleaf")
 (version "1.0.3")
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
        ocaml-graph
        ocaml5.0-eio-main
        tacaml
        ocaml-fileutils
        ocaml-yojson
        ocaml-uuidm
        ocaml-tyxml
        ocaml-alcotest
        ;; System dependencies
        pkg-config
        zlib
        openssl
        curl
        git
        ;; Frontend
        longleaf-frontend-dev
        ;; Analytics server
        longleaf-quantstats-dev))
 (home-page "https://github.com/hesterjeng/longleaf")
 (synopsis "Algorithmic trading platform written in OCaml")
 (description
  "Longleaf is an algorithmic trading platform that supports live trading,
paper trading, and backtesting with multiple brokerages and market data sources.
The platform uses a functional, modular architecture with strategies implemented
as functors for maximum code reuse and type safety.

The platform includes tacaml for TA-Lib technical analysis bindings.")
 (license license:gpl3+))
