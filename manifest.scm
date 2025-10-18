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
             (gnu packages ocaml)
             (gnu packages longleaf)
             (gnu packages node))

;; Create manifest with development packages and system dependencies
;; (packages->manifest
;;  (list ;; Frontend and tearsheets
;;   ;; longleaf-quantstats-dev
;;   ;; longleaf-frontend-dev
;;   ;; Python web framework
;;   python
;;   direnv
;;   gmp
;;   python-fastapi
;;   python-uvicorn
;;   python-frozendict
;;   python-wrapper
;;   python-peewee
;;   python-ipython
;;   nlopt
;;   ta-lib
;;   libev
;;   ;; Process orchestration
;;   tmux
;;   ;; System dependencies
;;   pkg-config
;;   zlib
;;   openssl
;;   curl
;;   git
;;   ))
(specifications->manifest
  (list "emacs-lsp-mode"
        "emacs-lsp-ui"
        "emacs-tuareg"
        "ocaml-merlin"
        "ocaml-lsp-server"
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
        "ocaml-graph"
        "ocaml-eio-main"
        "ocaml-tacaml"
        "ocaml-fileutils"
        "ocaml-yojson"
        "ocaml-uuidm"
        "ocaml-tyxml"
        "ocaml-cohttp-eio"
        "ocaml-ppx-yojson-conv"
        "python-quantstats"
        "python-yfinance"
        "python-multitasking"
        "nlopt"
        "node"))
