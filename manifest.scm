;; Guix manifest for Longleaf development environment
;; Load Python server and React frontend packages

(use-modules (guix packages)
             (gnu packages)
             (gnu packages compression)
             (gnu packages tls)
             (gnu packages curl)
             (gnu packages version-control)
             (gnu packages pkg-config)
             (gnu packages terminals)
             (gnu packages tmux)
             (gnu packages shellutils)
             (gnu packages maths)
             (gnu packages finance)
             (gnu packages databases)
             (gnu packages libevent)
             (gnu packages python-web)
             (gnu packages python-science)
             (gnu packages multiprecision)
             (gnu packages longleaf-ocaml)
             (gnu packages longleaf)
             (gnu packages node))

(specifications->manifest
 (list
        "ocaml"
        "dune"
        "ocamlformat"
        "ocaml-nlopt"
        "ocaml-ptime"
        "ocaml-ppx-yojson-conv-lib"
        "ocaml-ppx-deriving"
        "ocaml-ppx-variants-conv"
        "ocaml-ppx-fields-conv"
        "ocaml-cmdliner"
	"ocaml-backoff"
        "ocaml-graph"
        "ocaml-eio-main"
        "ocaml-tacaml"
        "ocaml-saturn"
        "ocaml-fileutils"
        "ocaml-yojson"
        "ocaml-uuidm"
        "ocaml-tyxml"
        "ocaml-cohttp-eio"
        "ocaml-ppx-yojson-conv"
        "ocaml-tls"
        "ocaml-x509"
	"ocaml-ca-certs"
        "ocaml-mirage-crypto-rng"
	"ocaml-tls-eio"
        "ocaml-base64"
        "python-quantstats"
        "python-yfinance"
        "python-multitasking"
	"python-fastapi"
        "nlopt"
        "node"))
