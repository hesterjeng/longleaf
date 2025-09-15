;; Guix manifest for Longleaf development environment
;; Load Python server and React frontend packages

(use-modules (guix packages)
             (gnu packages)
             (gnu packages compression)
             (gnu packages tls)
             (gnu packages curl)
             (gnu packages version-control)
             (gnu packages pkg-config))

;; Load QuantStats server package
(load "tearsheets/longleaf-quantstats.scm")

;; Load React frontend package  
(load "react/longleaf-frontend.scm")

;; Warning message
(format #t "~%WARNING: This manifest provides frontend and tearsheets dependencies only.~%")
(format #t "For OCaml development, use: guix shell -f longleaf.scm~%~%")

;; Create manifest with development packages and system dependencies
(packages->manifest
 (list ;; Frontend and tearsheets
       longleaf-quantstats-dev
       longleaf-frontend-dev
       ;; Process orchestration  
       overmind
       ;; System dependencies
       pkg-config
       zlib
       openssl
       curl
       git))