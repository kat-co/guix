;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages audio)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib) ;dbus
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)  ;libsndfile, libsamplerate
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml))

(define-public aubio
  (package
    (name "aubio")
    (version "0.4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://aubio.org/pub/aubio-" version ".tar.bz2"))
             (sha256
              (base32
               "15f6nf76y7iyl2kl4ny7ky0zpxfxr8j3902afvd6ydnnkh5dzmr5"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:configure-flags
       '("--enable-fftw3f"
         "--enable-jack"
         "--enable-sndfile"
         "--enable-samplerate"
         ;; enable compilation with avcodec once available
         "--disable-avcodec")
       #:python ,python-2))
    (inputs
     `(("jack" ,jack-1)
       ("libuuid" ,util-linux)
       ("libsndfile" ,libsndfile)
       ("libsamplerate" ,libsamplerate)
       ("fftwf" ,fftwf)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://aubio.org/")
    (synopsis "A library for audio labelling")
    (description
     "aubio is a tool designed for the extraction of annotations from audio
signals.  Its features include segmenting a sound file before each of its
attacks, performing pitch detection, tapping the beat and producing MIDI
streams from live audio.")
    (license license:gpl3+)))

(define-public jack-1
  (package
    (name "jack")
    (version "0.124.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://jackaudio.org/downloads/jack-audio-connection-kit-"
                   version
                   ".tar.gz"))
             (sha256
              (base32
               "1mk1wnx33anp6haxfjjkfhwbaknfblsvj35nxvz0hvspcmhdyhpb"))))
    (build-system gnu-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("libuuid" ,util-linux)))
    (home-page "http://jackaudio.org/")
    (synopsis "JACK audio connection kit")
    (description
     "JACK is a low-latency audio server.  It can connect a number of
different applications to an audio device, as well as allowing them to share
audio between themselves.  JACK is different from other audio server efforts
in that it has been designed from the ground up to be suitable for
professional audio work.  This means that it focuses on two key areas:
synchronous execution of all clients, and low latency operation.")
    ;; Most files are licensed under the GPL. However, the libjack/ tree is
    ;; licensed under the LGPL in order to allow for proprietary usage.
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public jack-2
  (package (inherit jack-1)
    (name "jack")
    (version "1.9.10")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/jackaudio/jack2/archive/v"
                   version
                   ".tar.gz"))
             (sha256
              (base32
               "03b0iiyk3ng3vh5s8gaqwn565vik7910p56mlbk512bw3dhbdwc8"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:configure-flags '("--dbus"
                           "--alsa")))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("expat" ,expat)
       ("libsamplerate" ,libsamplerate)
       ("opus" ,opus)
       ("readline" ,readline)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    ;; Most files are under GPLv2+, but some headers are under LGPLv2.1+
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public ladspa
  (package
    (name "ladspa")
    (version "1.13")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.ladspa.org/download/ladspa_sdk_"
                   version
                   ".tgz"))
             (sha256
              (base32
               "0srh5n2l63354bc0srcrv58rzjkn4gv8qjqzg8dnq3rs4m7kzvdm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; the "test" target is a listening test only
       #:phases
       (alist-replace
        'configure
        (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
          (chdir "src")
          (let ((out (assoc-ref outputs "out")))
            (substitute* "makefile"
              (("/usr/lib/ladspa/") (string-append out "/lib/ladspa/"))
              (("/usr/include/")    (string-append out "/include/"))
              (("/usr/bin/")        (string-append out "/bin/"))
              (("-mkdirhier")       "mkdir -p")
              (("^CC.*")            "CC = gcc\n")
              (("^CPP.*")           "CPP = g++\n"))))
        (alist-delete 'build %standard-phases))))
    (home-page "http://ladspa.org")
    (synopsis "Linux Audio Developer's Simple Plugin API (LADSPA)")
    (description
     "LADSPA is a standard that allows software audio processors and effects
to be plugged into a wide range of audio synthesis and recording packages.")
    (license license:lgpl2.1+)))

(define-public liblo
  (package
    (name "liblo")
    (version "0.28")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/liblo/liblo-"
                   version
                   ".tar.gz"))
             (sha256
              (base32
               "02drgnpirvl2ihvzgsmn02agr5sj3vipzzw9vma56qlkgfvak56s"))))
    (build-system gnu-build-system)
    (arguments
     `(;; liblo test FAILED
       ;; liblo server error 19 in setsockopt(IP_ADD_MEMBERSHIP): No such device
       #:tests? #f))
    (home-page "http://liblo.sourceforge.net")
    (synopsis "Implementation of the Open Sound Control protocol")
    (description
     "liblo is a lightweight library that provides an easy to use
implementation of the Open Sound Control (OSC) protocol.")
    (license license:lgpl2.1+)))

(define-public lilv
  (package
    (name "lilv")
    (version "0.20.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.drobilla.net/lilv-"
                                 version
                                 ".tar.bz2"))
             (sha256
              (base32
               "0aj2plkx56iar8vzjbq2l7hi7sp0ml99m0h44rgwai2x4vqkk2j2"))))
    (build-system waf-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (inputs
     `(("lv2" ,lv2)
       ("serd" ,serd)
       ("sord" ,sord)
       ("sratom" ,sratom)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://drobilla.net/software/lilv/")
    (synopsis "Library to simplify use of LV2 plugins in applications")
    (description
     "Lilv is a C library to make the use of LV2 plugins as simple as possible
for applications.  Lilv is the successor to SLV2, rewritten to be
significantly faster and have minimal dependencies.")
    (license license:isc)))

(define-public lv2
  (package
    (name "lv2")
    (version "1.10.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://lv2plug.in/spec/lv2-"
                                 version
                                 ".tar.bz2"))
             (sha256
              (base32
               "1md41x9snrp4mcfyli7lyfpvcfa78nfy6xkdy84kppnl8m5qw378"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:configure-flags '("--lv2-system")))
    (inputs
     ;; Leaving off cairo and gtk+-2.0 which are needed for example plugins
     `(("libsndfile" ,libsndfile)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://lv2plug.in/")
    (synopsis "LV2 audio plugin specification")
    (description
     "LV2 is an open specification for audio plugins and host applications.
At its core, LV2 is a simple stable interface, accompanied by extensions which
add functionality to support the needs of increasingly powerful audio
software.")
    (license license:isc)))

(define-public sratom
  (package
    (name "sratom")
    (version "0.4.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.drobilla.net/sratom-"
                                 version
                                 ".tar.bz2"))
             (sha256
              (base32
               "080jjiyxjnj7hf25844hd9rb01grvzz1rk8mxcdnakywmspbxfd4"))))
    (build-system waf-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (inputs
     `(("lv2" ,lv2)
       ("serd" ,serd)
       ("sord" ,sord)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://drobilla.net/software/sratom/")
    (synopsis "Library for serialising LV2 atoms to/from RDF")
    (description
     "Sratom is a library for serialising LV2 atoms to/from RDF, particularly
the Turtle syntax.")
    (license license:isc)))

(define-public suil
  (package
    (name "suil")
    (version "0.8.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.drobilla.net/suil-"
                                 version
                                 ".tar.bz2"))
             (sha256
              (base32
               "1s3adyiw7sa5gfvm5wasa61qa23629kprxyv6w8hbxdiwp0hhxkq"))))
    (build-system waf-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (inputs
     `(("lv2" ,lv2)
       ("gtk+-2" ,gtk+-2)
       ("qt-4" ,qt-4)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://drobilla.net/software/suil/")
    (synopsis "Library for loading and wrapping LV2 plugin UIs")
    (description
     "Suil is a lightweight C library for loading and wrapping LV2 plugin UIs.

Suil makes it possible to load a UI of a toolkit in a host using another
toolkit.  The API is designed such that hosts do not need to explicitly
support specific toolkits – if Suil supports a particular toolkit, then UIs in
that toolkit will work in all hosts that use Suil automatically.

Suil currently supports every combination of Gtk 2, Qt 4, and X11.")
    (license license:isc)))

(define-public vamp
  (package
    (name "vamp")
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://code.soundsoftware.ac.uk"
                    "/attachments/download/690/vamp-plugin-sdk-"
                    version
                    ".tar.gz"))
             (sha256
              (base32
               "178kfgq08cmgdzv7g8dwyjp4adwx8q04riimncq4nqkm8ng9ywbv"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (inputs
     `(("libsndfile" ,libsndfile)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://vamp-plugins.org")
    (synopsis "Modular and extensible audio processing system")
    (description
     "Vamp is an audio processing plugin system for plugins that extract
descriptive information from audio data — typically referred to as audio
analysis plugins or audio feature extraction plugins.")
    (license
     (license:x11-style
      "https://code.soundsoftware.ac.uk/projects/vamp-plugin-sdk/repository/entry/COPYING"))))
