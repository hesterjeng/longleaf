;; Longleaf QuantStats Server Package Definition
(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix gexp)
             (guix build-system python)
             (guix build-system pyproject)
             (guix build-system copy)
             ((guix licenses) #:prefix license:)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages python-science)
             (gnu packages python-web)
             (gnu packages python-build)
             (gnu packages time)
             (gnu packages xml)
             (gnu packages protobuf)
             (gnu packages statistics)
             (gnu packages libffi)
             (gnu packages tls)
             (gnu packages python-crypto)
             (gnu packages databases))



;; multitasking - Non-blocking Python methods using decorators
(define-public python-multitasking
  (package
   (name "python-multitasking")
   (version "0.0.12")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://files.pythonhosted.org/packages/source/m/multitasking/multitasking-"
                         version ".tar.gz"))
     (sha256
      (base32 "1lc4kcs5fnhp2rrr4izjnviqsrbx3k27vpf54zi8ajwcxnl2zfig"))))
   (build-system python-build-system)
   (arguments
    '(#:tests? #f))  ; No tests included in source
   (home-page "https://github.com/ranaroussi/multitasking")
   (synopsis "Non-blocking Python methods using decorators")
   (description
    "MultiTasking is a lightweight Python library that lets you convert your
Python methods into asynchronous, non-blocking methods simply by using a
decorator.  Perfect for I/O-bound tasks, API calls, web scraping, and any
scenario where you want to run multiple operations concurrently without the
complexity of manual thread or process management.")
   (license license:asl2.0)))

;; yfinance - Market data downloader
;; NOTE: Pinned to v0.2.57 to avoid curl_cffi dependency introduced in v0.2.58+
(define-public python-yfinance
  (package
   (name "python-yfinance")
   (version "0.2.57")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://files.pythonhosted.org/packages/source/y/yfinance/yfinance-"
                         version ".tar.gz"))
     (sha256
      (base32 "1cgkch19a1rn175ixd8180a47dnc8nmwljyj2538s55ijyd385kv"))))
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
          python-protobuf
          python-frozendict
          python-peewee))
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
          python-yfinance
          python-multitasking
          python-ipython))
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
   (build-system pyproject-build-system)
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
          python-quantstats
          python-pandas
          python-numpy
          python-frozendict))
   (home-page "https://github.com/hesterjeng/longleaf")
   (synopsis "Longleaf QuantStats server development environment")
   (description
    "Development package for Longleaf QuantStats FastAPI server. Provides Python
environment with all dependencies for portfolio analytics and reporting.")
   (license license:gpl3+)))

;; Default export for development
longleaf-quantstats-dev
