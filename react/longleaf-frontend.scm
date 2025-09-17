;; Longleaf React Frontend Package Definition
(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix gexp)
             (guix build-system copy)
             (guix build-system gnu)
             (guix build utils)
             ((guix licenses) #:prefix license:)
             (gnu packages)
             (gnu packages node))

;; Development package - provides Node.js environment for npm workflow
(define-public longleaf-frontend-dev
  (package
    (name "longleaf-frontend-dev")
    (version "0.1.0")
    (source (local-file "." "longleaf-frontend-source"
                        #:recursive? #t
                        #:select? (lambda (file stat)
                                    (not (string-contains file "node_modules")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "." (string-append out "/share/longleaf-frontend"))
               #t))))))
    (propagated-inputs
     (list node))
    (home-page "https://github.com/hesterjeng/longleaf")
    (synopsis "Longleaf React frontend development environment")
    (description
     "Development package for Longleaf React frontend. Provides Node.js and npm
for running 'npm install' and 'npm start' in the React directory.")
    (license license:gpl3+)))

;; Production package - built static files
(define-public longleaf-frontend
  (package
    (name "longleaf-frontend")
    (version "0.1.0")
    (source (local-file "build" "longleaf-frontend-build"
                        #:recursive? #t))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/longleaf/static"))))
    (home-page "https://github.com/hesterjeng/longleaf")
    (synopsis "Longleaf React frontend (built)")
    (description
     "Pre-built React dashboard for Longleaf trading platform. Contains
static HTML, CSS, and JavaScript files ready for serving.")
    (license license:gpl3+)))

;; Default export for development
longleaf-frontend-dev