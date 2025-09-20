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
             (gnu packages libevent)
             (gnu packages python-web)
             (gnu packages python-science)
             (gnu packages multiprecision)
             (gnu packages node))

;; Load QuantStats server package
(load "tearsheets/longleaf-quantstats.scm")

;; Load React frontend package  
(load "react/longleaf-frontend.scm")

;; Warning message
(format #t "~%WARNING: This manifest provides frontend and tearsheets dependencies only.~%")
(format #t "~%OCaml is not handled by Guix yet.~%")

;; Create manifest with development packages and system dependencies
(packages->manifest
 (list ;; Frontend and tearsheets
  longleaf-quantstats-dev
  longleaf-frontend-dev
  ;; Python web framework
  python
  direnv
  gmp
  python-fastapi
  python-uvicorn
  python-wrapper
  nlopt
  libev
  ;; Process orchestration
  tmux
  ;; System dependencies
  pkg-config
  zlib
  openssl
  curl
  git))
