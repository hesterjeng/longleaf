(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system dune)
             ((guix licenses) #:prefix license:)
             (gnu packages ocaml)
             (gnu packages pkg-config)
             (gnu packages maths))

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
        ocaml-ctypes
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
 (license license:gpl3+))