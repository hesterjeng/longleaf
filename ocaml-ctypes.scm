(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system dune)
             ((guix licenses) #:prefix license:)
             (gnu packages libffi)
             (gnu packages ocaml)
             (gnu packages pkg-config))

(define-public ocaml-ctypes-latest
  (package
   (name "ocaml-ctypes")
   (version "0.21.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/yallop/ocaml-ctypes/archive/refs/tags/"
                         version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1zqlgv5f63pcj9dqfi29my9fblw0jpynff6zsxgbf0n24q7knavg"))))
   (build-system dune-build-system)
   (arguments
    `(#:tests? #f))
   (native-inputs
    (list ocaml-ounit))
   (propagated-inputs
    (list ocaml-bigarray-compat
          ocaml-integers
          libffi
          pkg-config))
   (home-page "https://github.com/yallop/ocaml-ctypes")
   (synopsis "Library for binding to C libraries using pure OCaml")
   (description
    "Ctypes is a library for binding to C libraries using pure OCaml.  The
primary aim is to make writing C extensions as straightforward as possible.
The core of ctypes is a set of combinators for describing the structure of C
types -- numeric types, arrays, pointers, structs, unions and functions.  You
can use these combinators to describe the types of the functions that you want
to call, then bind directly to those functions -- all without writing or
generating any C!")
   (license license:expat)))