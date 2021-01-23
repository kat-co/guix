;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020 Helio Machado <0x2b3bfa0+guix@googlemail.com>
;;; Copyright © 2021 François Joulaud <francois.joulaud@radiofrance.com>
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

;;; (guix import golang) wants to make easier to create Guix package
;;; declaration for Go modules.
;;;
;;; Modules in Go are "collection of related Go packages" which are
;;; "the unit of source code interchange and versioning".
;;; Modules are generally hosted in a repository.
;;;
;;; At this point it should handle correctly modules which
;;; - have only Go dependencies;
;;; - use go.mod;
;;; - and are accessible from proxy.golang.org (or configured GOPROXY).
;;;
;;; We translate Go module paths  to a Guix package name under the
;;; assumption that there will be no collision.

(define-module (guix import go)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (json)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module ((guix build download) #:prefix build-download:)
  #:use-module (web uri)

  #:export (go-module->guix-package
            go-module-recursive-import
            infer-module-root))

(define (go-path-escape path)
  "Escape a module path by replacing every uppercase letter with an exclamation
mark followed with its lowercase equivalent, as per the module Escaped Paths
specification. https://godoc.org/golang.org/x/mod/module#hdr-Escaped_Paths"
  (define (escape occurrence)
    (string-append "!" (string-downcase (match:substring occurrence))))
  (regexp-substitute/global #f "[A-Z]" path 'pre escape 'post))


(define (fetch-latest-version goproxy-url module-path)
  "Fetches the version number of the latest version for MODULE-PATH from the
given GOPROXY-URL server."
  (assoc-ref
   (json-fetch (format #f "~a/~a/@latest" goproxy-url
                       (go-path-escape module-path)))
   "Version"))

(define (fetch-go.mod goproxy-url module-path version file)
  "Fetches go.mod from the given GOPROXY-URL server for the given MODULE-PATH
and VERSION."
  (let ((url (format #f "~a/~a/@v/~a.mod" goproxy-url
                     (go-path-escape module-path)
                     (go-path-escape version))))
    (parameterize ((current-output-port (current-error-port)))
      (build-download:url-fetch url
                                file
                                #:print-build-trace? #f))))

(define (parse-go.mod go.mod-path)
  "PARSE-GO.MOD takes a filename in GO.MOD-PATH and extract a list of
requirements from it."
  ;; We parse only a subset of https://golang.org/ref/mod#go-mod-file-grammar
  ;; which we think necessary for our use case.
  (define (toplevel results)
    "Main parser, RESULTS is a pair of alist serving as accumulator for
     all encountered requirements and replacements."
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        ;; parsing ended, give back the result
        results)
       ((string=? line "require (")
        ;; a require block begins, delegate parsing to IN-REQUIRE
        (in-require results))
       ((string-prefix? "require " line)
        ;; a require directive by itself
        (let* ((stripped-line (string-drop line 8))
               (new-results (require-directive results stripped-line)))
          (toplevel new-results)))
       ((string-prefix? "replace " line)
        ;; a replace directive by itself
        (let* ((stripped-line (string-drop line 8))
               (new-results (replace-directive results stripped-line)))
          (toplevel new-results)))
       (#t
        ;; unrecognised line, ignore silently
        (toplevel results)))))
  (define (in-require results)
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        ;; this should never happen here but we ignore silently
        results)
       ((string=? line ")")
        ;; end of block, coming back to toplevel
        (toplevel results))
       (#t
        (in-require (require-directive results line))))))
  (define (replace-directive results line)
    "Extract replaced modules and new requirements from replace directive
    in LINE and add to RESULTS."
    ;; ReplaceSpec = ModulePath [ Version ] "=>" FilePath newline
    ;;             | ModulePath [ Version ] "=>" ModulePath Version newline .
    (let* ((requirements (car results))
           (replaced (cdr results))
           (re (string-concatenate
                '("([^[:blank:]]+)([[:blank:]]+([^[:blank:]]+))?"
                  "[[:blank:]]+" "=>" "[[:blank:]]+"
                  "([^[:blank:]]+)([[:blank:]]+([^[:blank:]]+))?")))
           (match (string-match re line))
           (module-path (match:substring match 1))
           (version (match:substring match 3))
           (new-module-path (match:substring match 4))
           (new-version (match:substring match 6))
           (new-replaced (acons module-path version replaced))
           (new-requirements
            (if (string-match "^\\.?\\./" new-module-path)
                requirements
                (acons new-module-path new-version requirements))))
      (cons new-requirements new-replaced)))
  (define (require-directive results line)
    "Extract requirement from LINE and add it to RESULTS."
    (let* ((requirements (car results))
           (replaced (cdr results))
           ;; A line in a require directive is composed of a module path and
           ;; a version separated by whitespace and an optionnal '//' comment at
           ;; the end.
           (re (string-concatenate
                '("^[[:blank:]]*"
                  "([^[:blank:]]+)[[:blank:]]+([^[:blank:]]+)"
                  "([[:blank:]]+//.*)?")))
           (match (string-match re line))
           (module-path (match:substring match 1))
           (version (match:substring match 2)))
      (cons (acons module-path version requirements) replaced)))
  (with-input-from-file go.mod-path
    (lambda ()
      (let* ((results (toplevel '(() . ())))
             (requirements (car results))
             (replaced (cdr results)))
        ;; At last we remove replaced modules from the requirements list
        (fold
         (lambda (replacedelem requirements)
             (alist-delete! (car replacedelem) requirements))
         requirements
         replaced)))))

(define (infer-module-root module-path)
  "Go modules can be defined at any level of a repository's tree, but querying
for the meta tag usually can only be done at the webpage at the root of the
repository. Therefore, it is sometimes necessary to try and derive a module's
root path from its path. For a set of well-known forges, the pattern of what
consists of a module's root page is known before hand."
  ;; See the following URL for the official Go equivalent:
  ;; https://github.com/golang/go/blob/846dce9d05f19a1f53465e62a304dea21b99f910/src/cmd/go/internal/vcs/vcs.go#L1026-L1087
  ;;
  ;; FIXME: handle module path with VCS qualifier as described in
  ;; https://golang.org/ref/mod#vcs-find and
  ;; https://golang.org/cmd/go/#hdr-Remote_import_paths
  (define-record-type <vcs>
    (make-vcs url-prefix root-regex type)
    vcs?
    (url-prefix vcs-url-prefix)
    (root-regex vcs-root-regex)
    (type vcs-type))
  (let* ((known-vcs
          (list
           (make-vcs
            "github.com"
            "^(github\\.com/[A-Za-z0-9_.\\-]+/[A-Za-z0-9_.\\-]+)(/[A-Za-z0-9_.\\-]+)*$"
            'git)
           (make-vcs
            "bitbucket.org"
            "^(bitbucket\\.org/([A-Za-z0-9_.\\-]+/[A-Za-z0-9_.\\-]+))(/[A-Za-z0-9_.\\-]+)*$`"
            'unknown)
           (make-vcs
            "hub.jazz.net/git/"
            "^(hub\\.jazz\\.net/git/[a-z0-9]+/[A-Za-z0-9_.\\-]+)(/[A-Za-z0-9_.\\-]+)*$"
            'git)
           (make-vcs
            "git.apache.org"
            "^(git\\.apache\\.org/[a-z0-9_.\\-]+\\.git)(/[A-Za-z0-9_.\\-]+)*$"
            'git)
           (make-vcs
            "git.openstack.org"
            "^(git\\.openstack\\.org/[A-Za-z0-9_.\\-]+/[A-Za-z0-9_.\\-]+)(\\.git)?(/[A-Za-z0-9_.\\-]+)*$"
            'git)))
         (vcs (find (lambda (vcs) (string-prefix? (vcs-url-prefix vcs) module-path))
                    known-vcs)))
    (if vcs
        (match:substring (string-match (vcs-root-regex vcs) module-path) 1)
        module-path)))

(define (to-guix-package-name module-path)
  "Converts a module's path to the canonical Guix format for Go packages."
  (string-downcase
   (string-append "go-"
                  (string-replace-substring
                   (string-replace-substring
                    module-path
                    "." "-")
                   "/" "-"))))

(define (fetch-module-meta-data module-path)
  "Fetches module meta-data from a module's landing page. This is necessary
because goproxy servers don't currently provide all the information needed to
build a package."
  ;; FIXME: This code breaks on k8s.io which have a meta tag splitted
  ;; on several lines
  (let* ((port (build-download:http-fetch (string->uri (format #f "https://~a?go-get=1" module-path))))
         (module-metadata #f)
         (meta-tag-prefix "<meta name=\"go-import\" content=\"")
         (meta-tag-prefix-length (string-length meta-tag-prefix)))
    (do ((line (read-line port) (read-line port)))
        ((or (eof-object? line)
             module-metadata))
      (let ((meta-tag-index (string-contains line meta-tag-prefix)))
        (when meta-tag-index
          (let* ((start (+ meta-tag-index meta-tag-prefix-length))
                 (end (string-index line #\" start)))
            (set! module-metadata
                  (string-split (substring/shared line start end) #\space))))))
    (close-port port)
    module-metadata))

(define (module-meta-data-scs meta-data)
  "Return the source control system specified by a module's meta-data."
  (string->symbol (list-ref meta-data 1)))

(define (module-meta-data-repo-url meta-data goproxy-url)
  "Return the URL where the fetcher which will be used can download the source
control."
  (if (member (module-meta-data-scs meta-data) '(fossil mod))
      goproxy-url
      (list-ref meta-data 2)))

(define (source-uri scs-type scs-repo-url file)
  "Generate the `origin' block of a package depending on what type of source
control system is being used."
  (case scs-type
    ((git)
     `(origin
        (method git-fetch)
        (uri (git-reference
              (url ,scs-repo-url)
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          ,(guix-hash-url file)))))
    ((hg)
     `(origin
        (method hg-fetch)
        (uri (hg-reference
              (url ,scs-repo-url)
              (changeset ,version)))
        (file-name (format #f "~a-~a-checkout" name version))))
    ((svn)
     `(origin
        (method svn-fetch)
        (uri (svn-reference
              (url ,scs-repo-url)
              (revision (string->number version))
              (recursive? #f)))
        (file-name (format #f "~a-~a-checkout" name version))
        (sha256
         (base32
          ,(guix-hash-url file)))))
    (else
     (raise-exception (format #f "unsupported scs type: ~a" scs-type)))))

(define* (go-module->guix-package module-path #:key (goproxy-url "https://proxy.golang.org"))
  (call-with-temporary-output-file
   (lambda (temp port)
     (let* ((latest-version (fetch-latest-version goproxy-url module-path))
            (go.mod-path (fetch-go.mod goproxy-url module-path latest-version
                                       temp))
            (dependencies (map car (parse-go.mod temp)))
            (guix-name (to-guix-package-name module-path))
            (root-module-path (infer-module-root module-path))
            ;; VCS type and URL are not included in goproxy information. For
            ;; this we need to fetch it from the official module page.
            (meta-data (fetch-module-meta-data root-module-path))
            (scs-type (module-meta-data-scs meta-data))
            (scs-repo-url (module-meta-data-repo-url meta-data goproxy-url)))
       (values
        `(package
           (name ,guix-name)
           ;; Elide the "v" prefix Go uses
           (version ,(string-trim latest-version #\v))
           (source
            ,(source-uri scs-type scs-repo-url temp))
           (build-system go-build-system)
           ,@(maybe-inputs (map to-guix-package-name dependencies))
           ;; TODO(katco): It would be nice to make an effort to fetch this
           ;; from known forges, e.g. GitHub
           (home-page ,(format #f "https://~a" root-module-path))
           (synopsis "A Go package")
           (description ,(format #f "~a is a Go package." guix-name))
           (license #f))
        dependencies)))))

(define* (go-module-recursive-import package-name
                                     #:key (goproxy-url "https://proxy.golang.org"))
  (recursive-import
   package-name
   #:repo->guix-package (lambda* (name . _)
                          (go-module->guix-package
                           name
                           #:goproxy-url goproxy-url))
   #:guix-name to-guix-package-name))
