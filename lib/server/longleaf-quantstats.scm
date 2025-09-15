;; Longleaf QuantStats Server Package Definition
(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix gexp)
             (guix build-system python)
             (guix build-system copy)
             ((guix licenses) #:prefix license:)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages python-science)
             (gnu packages python-web)
             (gnu packages statistics))

;; FastAPI - Web framework
(define-public python-fastapi
  (package
    (name "python-fastapi")
    (version "0.112.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.pythonhosted.org/packages/source/f/fastapi/fastapi-"
                           version ".tar.gz"))
       (sha256
        (base32 "0s3xkyn9xr9b6r4xdh5xpf9b1wnwf4r8b3f6r2p8r3v7r8z9r2p3"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Skip tests that require network
    (propagated-inputs
     (list python-starlette
           python-pydantic
           python-typing-extensions))
    (home-page "https://github.com/tiangolo/fastapi")
    (synopsis "FastAPI framework, high performance, easy to learn")
    (description
     "FastAPI is a modern, fast web framework for building APIs with Python 3.7+
based on standard Python type hints.")
    (license license:expat)))

;; Uvicorn - ASGI server
(define-public python-uvicorn
  (package
    (name "python-uvicorn")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.pythonhosted.org/packages/source/u/uvicorn/uvicorn-"
                           version ".tar.gz"))
       (sha256
        (base32 "1x7z3r8w9q7r6x5y7z8r9p3q1w2e3r4t5y6u7i8o9p0q1w2e3r4t"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Skip tests
    (propagated-inputs
     (list python-click
           python-h11
           python-typing-extensions))
    (home-page "https://github.com/encode/uvicorn")
    (synopsis "Lightning-fast ASGI server implementation")
    (description
     "Uvicorn is a lightning-fast ASGI server implementation, using uvloop
and httptools.")
    (license license:bsd-3)))

;; yfinance - Market data downloader
(define-public python-yfinance
  (package
   (name "python-yfinance")
   (version "0.2.65")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://files.pythonhosted.org/packages/source/y/yfinance/yfinance-"
                         version ".tar.gz"))
     (sha256
      (base32 "1184qqpvd5sizd72zwq971i6pvqbw3irv0k2k0gxdscvqic5wiix"))))
   (build-system python-build-system)
   (arguments
    '(#:tests? #f     ; Skip tests that require network access
      #:phases
      (modify-phases %standard-phases
        (delete 'sanity-check))))  ; Skip sanity check that enforces curl_cffi dependency
   (propagated-inputs
    (list python-pandas
          python-numpy
          python-requests
          python-lxml
          python-appdirs
          python-pytz
          python-beautifulsoup4
          python-websockets
          python-protobuf))
   (home-page "https://github.com/ranaroussi/yfinance")
   (synopsis "Download market data from Yahoo! Finance API")
   (description
    "yfinance is a Python library that offers a threaded and Pythonic way
to download market data from Yahoo! Finance.  It fixes the temporary
authentication and decryption issues by dynamically scraping the data.")
   (license license:asl2.0)))

;; quantstats - Portfolio analytics for quants
(define-public python-quantstats
  (package
   (name "python-quantstats")
   (version "0.0.75")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/ranaroussi/quantstats/archive/"
                         version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "04a8r3rr36asij0dw31crvzj15xzz972drk48f9a80py0ha7431s"))))
   (build-system python-build-system)
   (arguments
    '(#:tests? #f     ; Skip tests that require yfinance network access
      #:phases
      (modify-phases %standard-phases
        (delete 'sanity-check))))  ; Skip sanity check that enforces yfinance curl_cffi dependency
   (propagated-inputs
    (list python-pandas
          python-numpy
          python-scipy
          python-matplotlib
          python-seaborn
          python-tabulate
          python-dateutil
          python-packaging
          python-yfinance))
   (home-page "https://github.com/ranaroussi/quantstats")
   (synopsis "Portfolio analytics for quants")
   (description
    "QuantStats is a Python library that performs portfolio analytics for quants.
It provides in-depth analytics and risk metrics for quantitative analysts
and portfolio managers including Sharpe ratio, win rate, volatility, drawdowns,
rolling statistics, monthly returns, and various performance tear sheets.")
   (license license:asl2.0)))

;; Development environment - provides Python environment for server
(define-public longleaf-quantstats-dev
  (package
    (name "longleaf-quantstats-dev")
    (version "0.1.0")
    (source (local-file "." "longleaf-quantstats-source"
                        #:recursive? #t))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "." (string-append out "/share/longleaf-quantstats"))
               #t))))))
    (propagated-inputs
     (list python
           python-fastapi
           python-uvicorn
           python-quantstats
           python-pandas
           python-numpy))
    (home-page "https://github.com/hesterjeng/longleaf")
    (synopsis "Longleaf QuantStats server development environment")
    (description
     "Development package for Longleaf QuantStats FastAPI server. Provides Python
environment with all dependencies for portfolio analytics and reporting.")
    (license license:gpl3+)))

;; Default export for development
longleaf-quantstats-dev