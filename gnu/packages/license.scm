;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021 Michael Rohleder <mike@rohleder.de>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu packages license)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public perl-regexp-pattern-license
  (package
    (name "perl-regexp-pattern-license")
    (version "3.1.94")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JO/JONASS/Regexp-Pattern-License-"
             "v" version ".tar.gz"))
       (sha256
        (base32 "0kznpv628jrndn4nw646f6pl7yqvmacwljlygvsjfdkyh0i4sr2k"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-regexp-pattern" ,perl-regexp-pattern)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-strictures" ,perl-strictures-2)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "https://metacpan.org/release/Regexp-Pattern-License")
    (synopsis "Regular expressions for legal licenses")
    (description "Regexp::Pattern::License provides a hash of regular
expression patterns related to legal software licenses.

Regexp::Pattern is a convention for organizing reusable regex patterns.")
    (license gpl3+)))

(define-public perl-string-copyright
  (package
    (name "perl-string-copyright")
    (version "0.003006")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JO/JONASS/String-Copyright-"
             version ".tar.gz"))
       (sha256
        (base32
         "0fzymv065nn3glwnw34nkyadzw2dh4rcz8avmki4zrnk4k45m01a"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-number-range" ,perl-number-range)))
    (propagated-inputs
     `(("perl-exporter-tiny" ,perl-exporter-tiny)))
    (home-page "https://metacpan.org/release/String-Copyright")
    (synopsis "Representation of text-based copyright statements")
    (description "String::Copyright Parses common styles of copyright
statements and serializes in normalized format.")
    (license gpl3+)))

(define-public perl-software-license
  (package
    (name "perl-software-license")
    (version "0.103014")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/L/LE/LEONT/Software-License-"
             version ".tar.gz"))
       (sha256
        (base32
         "128pbm9pf5drakm9bpkifc1zg8f005xabfwzg21nc03m5mhfligb"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-try-tiny" ,perl-try-tiny)))
    (propagated-inputs
     `(("perl-data-section" ,perl-data-section)
       ("perl-text-template" ,perl-text-template)))
    (home-page "https://metacpan.org/release/Software-License")
    (synopsis "Templated software licenses")
    (description "This package provides templated software licenses.")
    (license (package-license perl))))

(define-public licensecheck
  (package
    (name "licensecheck")
    (version "3.0.37")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/J/JO/JONASS/App-Licensecheck-"
                    "v" version ".tar.gz"))
              (sha256
               (base32
                "12l83zf85zagpagizmzy3bwkc659sbzqf18cycx8g4h6d3mc5kqw"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-regexp-pattern" ,perl-regexp-pattern)
       ("perl-software-license" ,perl-software-license)
       ("perl-test-requires" ,perl-test-requires)
       ("perl-test-roo" ,perl-test-roo)
       ("perl-test-script" ,perl-test-script)
       ("perl-universal-require" ,perl-universal-require)
       ("perl-number-range" ,perl-number-range)
       ("perl-sub-quote" ,perl-sub-quote)))
    (propagated-inputs
     `(("perl-getopt-long-descriptive" ,perl-getopt-long-descriptive)
       ("perl-moo" ,perl-moo-2)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-path-iterator-rule" ,perl-path-iterator-rule)
       ("perl-path-tiny" ,perl-path-tiny)
       ("perl-pod-constants" ,perl-pod-constants)
       ("perl-regexp-pattern-license" ,perl-regexp-pattern-license)
       ("perl-sort-key" ,perl-sort-key)
       ("perl-strictures" ,perl-strictures-2)
       ("perl-string-copyright" ,perl-string-copyright)
       ("perl-string-escape" ,perl-string-escape)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-module-runtime" ,perl-module-runtime)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perllib (string-append out "/lib/perl5/site_perl/"
                                            ,(package-version perl))))
               (wrap-program (string-append out "/bin/licensecheck")
                 `("PERL5LIB" ":"
                   prefix (,(string-append perllib ":" (getenv "PERL5LIB")))))
               #t))))))
    (home-page "https://metacpan.org/release/App-Licensecheck")
    (synopsis "License checker for source files")
    (description "Licensecheck attempts to determine the license that applies
to each file passed to it, by searching the start of the file for text
belonging to various licenses.")
    (license (package-license perl))))

(define-public reuse
  (package
    (name "reuse")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "reuse" version))
       (sha256
        (base32 "11i1xjbwbqjipzpbrbnp110zx1m49khn6dl5z3mjkjaz9kr6bl2f"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (inputs
     `(("python-binaryornot" ,python-binaryornot)
       ("python-boolean.py" ,python-boolean.py)
       ("python-debian" ,python-debian)
       ("python-jinja2" ,python-jinja2)
       ("python-license-expression" ,python-license-expression)
       ("python-requests" ,python-requests)))
    (home-page "https://reuse.software/")
    (synopsis "Provide and verify copyright and licensing information")
    (description
     "The REUSE tool helps you achieve and confirm license compliance with the
@uref{https://reuse.software, REUSE specification}, a set of recommendations
for licensing Free Software projects.  REUSE makes it easy to declare the
licenses under which your works are released, especially when reusing software
from different projects released under different licenses.  It avoids reliance
on fuzzy heuristicts and allows both legal experts and computers to understand
how your project is licensed.  This allows generating a \"bill of materials\"
for software.

This tool downloads full license texts, adds copyright and license information
to file headers, and contains a linter to identify problems.  There are other
tools that have a lot more features and functionality surrounding the analysis
and inspection of copyright and licenses in software projects.  This one is
designed to be simple.")
    (license (list asl2.0 gpl3+))))
