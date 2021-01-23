;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (guix import go)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (rnrs base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-34)
  #:use-module (guix json)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:use-module (guix build download)
  #:use-module (web uri)

  #:export (go-module->guix-package
            go-module-recursive-import
            fetch-module-meta-data))

(define (escape-capital-letters s)
  "To avoid ambiguity when serving from case-insensitive file systems, the
$module and $version elements are case-encoded by replacing every uppercase
letter with an exclamation mark followed by the corresponding lower-case
letter."
  (let ((escaped-string (string)))
    (string-for-each-index
     (lambda (i)
       (let ((c (string-ref s i)))
         (set! escaped-string
           (string-concatenate
            (list escaped-string
                  (if (char-upper-case? c) "!" "")
                  (string (char-downcase c)))))))
     s)
    escaped-string))

(define (fetch-latest-version goproxy-url module-path)
  "Fetches the version number of the latest version for MODULE-PATH from the
given GOPROXY-URL server."
  (assoc-ref
   (json-fetch (format #f "~a/~a/@latest" goproxy-url
                       (escape-capital-letters module-path)))
   "Version"))

(define (fetch-go.mod goproxy-url module-path version file)
  "Fetches go.mod from the given GOPROXY-URL server for the given MODULE-PATH
and VERSION."
  (url-fetch (format #f "~a/~a/@v/~a.mod" goproxy-url
                     (escape-capital-letters module-path)
                     (escape-capital-letters version))
             file
             #:print-build-trace? #f))

(define (parse-go.mod go.mod-path)
  "Parses a go.mod file and returns an alist of module path to version."
  (define (relative? e) (string= e ".."))
  (define (resolve-replacement-path module-uri replacement-uri)
    ;; Replacements may be paths relative to the module's URI. Here we
    ;; resolve the relativity if necessary.
    (let* ((module-path (split-and-decode-uri-path module-uri))
           (replacement-path (split-and-decode-uri-path replacement-uri))
           (number-relative-dir-up (length (filter relative? replacement-path))))
      (if (> number-relative-dir-up 0)
          (encode-and-join-uri-path (append
                                     (take module-path (- (length module-path)
                                                          number-relative-dir-up))
                                     (remove relative? replacement-path)))
          replacement-uri)))
  (with-input-from-file go.mod-path
    (lambda ()
      (let ((in-require? #f)
            (in-replace? #f)
            (requirements (list))
            ;; (original-mod-path . (new-mod-path . version))
            (replacements (list)))
        (do ((line (read-line) (read-line)))
            ((eof-object? line))
          (set! line (string-trim line))
          ;; The parser is either entering, within, exiting, or after the
          ;; require block. The Go toolchain is trustworthy so edge-cases like
          ;; double-entry, etc. need not complect the parser.
          (cond
           ((string=? line "require (")
            (set! in-require? #t))
           ((string=? line "replace (")
            (set! in-replace? #t))
           ;; Closing out either a require or replace block (exclude blocks
           ;; are not handled by this parser).
           ((and in-require? (string=? line ")"))
            (when in-require? (set! in-require? #f))
            (when in-replace? (set! in-replace? #f)))
           ((or in-require? (string-prefix? "require" line))
            (let* ((require-offset (if in-require? 1 0))
                   (requirement (string-split line #\space))
                   ;; Modules should be unquoted
                   (module-path (string-delete #\" (list-ref requirement (- 1 require-offset))))
                   (version (list-ref requirement (- 2 require-offset))))
              (set! requirements (acons module-path version requirements))))
           ((or in-replace? (string-prefix? "replace" line))
            (let* ((replace-offset (if in-replace? 1 0))
                   (requirement (string-split line #\space))
                   (module-path (list-ref requirement (- 1 replace-offset)))
                   (new-module-path (resolve-replacement-path module-path
                                                              (list-ref requirement (- 3 replace-offset))))
                   (version? (> (length requirement) (- 4 replace-offset)))
                   (version (if version? (list-ref requirement 4) #f)))
              (set! replacements (acons module-path (cons new-module-path
                                                          version)
                                        replacements))))))
        ;; Make any replacements necessary
        (map
         (lambda (r)
           (let ((old-path (car r))
                 (new-path (cadr r))
                 (version (cddr r)))
             (set! requirements (assoc-remove! requirements old-path))
             (set! requirements (acons new-path version requirements))))
         replacements)
        requirements))))

(define (module-path-without-major-version module-path)
  "Go modules can be appended with a major version indicator,
e.g. /v3. Sometimes it is desirable to work with the root module path. For
instance, for a module path github.com/foo/bar/v3 this function returns
github.com/foo/bar."
  (let ((m (string-match "(.*)\\/v[0-9]+$" module-path)))
    (if m
        (match:substring m 1)
        module-path)))

(define (infer-module-root module-path)
  "Go modules can be defined at any level of a repository's tree, but querying
for the meta tag usually can only be done at the webpage at the root of the
repository. Therefore, it is sometimes necessary to try and derive a module's
root path from its path. For a set of well-known forges, the pattern of what
consists of a module's root page is known before hand."
  ;; See the following URL for the official Go equivalent:
  ;; https://github.com/golang/go/blob/846dce9d05f19a1f53465e62a304dea21b99f910/src/cmd/go/internal/vcs/vcs.go#L1026-L1087
  (define-record-type <scs>
    (make-scs url-prefix root-regex type)
    scs?
    (url-prefix	scs-url-prefix)
    (root-regex scs-root-regex)
    (type	scs-type))
  (let* ((known-scs
          (list
           (make-scs
            "github.com"
            "^(github\\.com/[A-Za-z0-9_.\\-]+/[A-Za-z0-9_.\\-]+)(/[A-Za-z0-9_.\\-]+)*$"
            'git)
           (make-scs
            "bitbucket.org"
            "^(bitbucket\\.org/([A-Za-z0-9_.\\-]+/[A-Za-z0-9_.\\-]+))(/[A-Za-z0-9_.\\-]+)*$`"
            'unknown)
           (make-scs
            "hub.jazz.net/git/"
            "^(hub\\.jazz\\.net/git/[a-z0-9]+/[A-Za-z0-9_.\\-]+)(/[A-Za-z0-9_.\\-]+)*$"
            'git)
           (make-scs
            "git.apache.org"
            "^(git\\.apache\\.org/[a-z0-9_.\\-]+\\.git)(/[A-Za-z0-9_.\\-]+)*$"
            'git)
           (make-scs
            "git.openstack.org"
            "^(git\\.openstack\\.org/[A-Za-z0-9_.\\-]+/[A-Za-z0-9_.\\-]+)(\\.git)?(/[A-Za-z0-9_.\\-]+)*$"
            'git)))
         (scs (find (lambda (scs) (string-prefix? (scs-url-prefix scs) module-path))
                    known-scs)))
    (if scs
        (match:substring (string-match (scs-root-regex scs) module-path) 1)
        module-path)))

(define (to-guix-package-name module-path)
  "Converts a module's path to the canonical Guix format for Go packages."
  (string-downcase
   (string-append "go-"
                  (string-replace-substring
                   (string-replace-substring
                    ;; Guix has its own field for version
                    (module-path-without-major-version module-path)
                    "." "-")
                   "/" "-"))))

(define* (fetch-module-meta-data module-path #:key (module-uri #f))
  "Fetches module meta-data from a module's landing page. This is necessary
because goproxy servers don't currently provide all the information needed to
build a package.

There are valid Go Module paths which when queried will not serve the
requisite meta tag. This function will recursively walk up the URI
path (e.g. https://foo.bar/baz/bamf?go-get=1 -> https://foo.bar/baz?go-get=1,
etc.) in an attempt to fetch a valid meta tag."
  (unless module-uri
    (set! module-uri (string->uri (format #f "https://~a?go-get=1" module-path))))
  (guard (c (#t
             ;;(format #t "Caugh exception ~a~%" c)
             (let* ((d (uri->string module-uri))
                    (path (uri-path module-uri))
                    (path-list (split-and-decode-uri-path path))
                    (broader-path (if (null? path-list)
                                      (list)
                                      (take path-list (- (length path-list) 1)))))
               (if (< (length broader-path) (length path-list))
                   (fetch-module-meta-data
                    module-path
                    #:module-uri
                    (build-uri (uri-scheme module-uri)
                               #:host (uri-host module-uri)
                               #:port (uri-port module-uri)
                               #:path (format #f "/~a"
                                              (encode-and-join-uri-path
                                               broader-path))
                               #:query (uri-query module-uri)))
                   (raise-exception c)))))
    ;; TODO: Meta tags reflect the URL, and so redirects give meta tags we don't know how to look for
    (let* ((port (http-fetch module-uri))
           (module-metadata #f)
           (meta-tag-prefix (format #f "<meta name=\"go-import\" content=\"~a "
                                    module-path))
           (meta-tag-prefix-length (string-length meta-tag-prefix)))
      (do ((line (read-line port) (read-line port)))
          ((or (eof-object? line)
               module-metadata))
        (let ((meta-tag-index (string-contains line meta-tag-prefix)))
          (when meta-tag-index
            (let* ((start (+ meta-tag-index meta-tag-prefix-length))
                   (end (string-index line #\" start))
                   (meta-data (string-split (substring/shared line start end) #\space)))
              (set! module-metadata meta-data)))))
      (close-port port)
      (unless module-metadata
        (raise-exception
         (format #f "meta-data not found for module \"~a\" at \"~a\""
                 module-path (uri->string module-uri))))
      module-metadata)))

(define (module-meta-data-scs meta-data)
  "Return the source control system specified by a module's meta-data."
  (string->symbol (list-ref meta-data 0)))

(define (module-meta-data-repo-url meta-data goproxy-url)
  "Return the URL where the fetcher which will be used can download the source
control."
  (if (member (module-meta-data-scs meta-data) '(fossil mod))
      goproxy-url
      (list-ref meta-data 1)))

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
            ;; SCS type and URL are not included in goproxy information. For
            ;; this we need to fetch it from the official module page.
            (meta-data (fetch-module-meta-data root-module-path))
            (foo (unless meta-data (raise-exception (format #f "bad meta-data for `~a`" module-path))))
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
  (recursive-import package-name #f
                    #:repo->guix-package
                    (lambda (name _)
                      (go-module->guix-package name
                                               #:goproxy-url goproxy-url))
                    #:guix-name to-guix-package-name))
