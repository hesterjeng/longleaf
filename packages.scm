;; Additional OCaml packages for Longleaf not available in Guix
(use-modules (guix packages)
             (guix download)
             (guix build-system dune)
             ((guix licenses) #:prefix license:)
             (gnu packages ocaml))

;; iter - functional iterators
(define-public ocaml-iter
  (package
   (name "ocaml-iter")
   (version "1.9")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://github.com/c-cube/iter/releases/download/v1.9/iter-1.9.tbz")
     (sha256
      (base32 "0s79qwsj0gjs37qib3vkly2a4iv78ylv4djc0nq0s39f2nxybafv"))))
   (build-system dune-build-system)
   (propagated-inputs (list ocaml-odoc))
   (native-inputs (list ocaml-qcheck-core ocaml-ounit2 ocaml-mdx))
   (home-page "https://github.com/c-cube/iter/")
   (synopsis
    "Simple abstraction over `iter` functions, intended to iterate efficiently on collections while performing some transformations")
   (description #f)
   (license license:bsd-2)))

;; containers - functional data structures
(define-public ocaml-containers
  (package
   (name "ocaml-containers")
   (version "3.16")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://github.com/c-cube/ocaml-containers/releases/download/v3.16/containers-3.16.tbz")
     (sha256
      (base32 "0yavdf3wsz71vprqw2qpwq5k2gqvacqklv0v1llx5h2r5v5r4y8y"))))
   (build-system dune-build-system)
   (propagated-inputs (list ocaml-either ocaml-dune-configurator ocaml-odoc))
   (native-inputs (list ocaml-qcheck-core
                        ocaml-yojson
                        ocaml-iter
                        ocaml-gen
                        ocaml-csexp
                        ocaml-uutf))
   (home-page "https://github.com/c-cube/ocaml-containers/")
   (synopsis
    "A modular, clean and powerful extension of the OCaml standard library")
   (description #f)
   (license license:bsd-2)))

;; landmarks - performance profiling
(define-public ocaml-landmarks
  (package
   (name "ocaml-landmarks")
   (version "1.5")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/LexiFi/landmarks/archive/refs/tags/v1.5.tar.gz")
     (sha256
      (base32 "1w3h35xkvpzdk03zld0g4bsvhpchgr98kpwgii4s4pdhnxgqjz50"))))
   (build-system dune-build-system)
   (propagated-inputs (list ocaml-odoc))
   (native-inputs (list ocaml-js-of-ocaml))
   (home-page "https://github.com/LexiFi/landmarks")
   (synopsis "A simple profiling library")
   (description
    "Landmarks is a simple profiling library for OCaml.  It provides primitives to
measure time spent in portion of instrumented code.  The instrumentation of the
code may either done by hand, automatically or semi-automatically using the ppx
pepreprocessor (see landmarks-ppx package).")
   (license license:expat)))

;; piaf - HTTP client library
(define-public ocaml-piaf
  (package
   (name "ocaml-piaf")
   (version "0.2.0")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://github.com/anmonteiro/piaf/releases/download/0.2.0/piaf-0.2.0.tbz")
     (sha256
      (base32 "1yvhfc8g4mclmddckivvbhc9n9zm0x8ff5k1v3kambigll4r1yh7"))))
   (build-system dune-build-system)
   (propagated-inputs (list ocaml-logs
                            ocaml-eio-ssl
                            ocaml-magic-mime
                            ocaml-ssl
                            ocaml-uri
                            ocaml-ipaddr
                            ocaml-httpun-eio
                            ocaml-gluten-eio
                            ocaml-h2-eio
                            ocaml-httpun-ws
                            ocaml-pecu
                            ocaml-prettym
                            ocaml-unstrctrd
                            ocaml5.0-eio-main
                            ocaml-odoc))
   (native-inputs (list ocaml-dune-site ocaml-alcotest))
   (home-page "https://github.com/anmonteiro/piaf")
   (synopsis "An HTTP library with HTTP/2 support written entirely in OCaml")
   (description
    "Piaf is an HTTP library and webserver written entirely in OCaml.")
   (license license:bsd-3)))
