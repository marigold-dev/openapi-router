(lang dune 2.8)

;; Copyright 2021/2022 Johns Hopkins University Applied Physics Laboratory

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(generate_opam_files true)

(name openapi_router)

(package
 (name openapi_router)
 (version 0.1.0)
 (synopsis "Http server agnostic Openapi documentation generation")
 (description
  "A functor to wrap your http server and produce an Openapi documentation pages")
 (license "BSD-3-Clause")
 (authors
  "J. Aaron Pendergrass <james.pendergrass@jhuapl.edu>"
  "Jules Guesnon <jules@matigold.dev>")
 (maintainers "Jules Guesnon <jules@matigold.dev>")
 (homepage "https://github.com/marigold-dev/openapi-router")
 (source
  (github "marigold-dev/openapi-router"))
 (bug_reports "https://github.com/marigold-dev/openapi-router/issues")
 (depends
  (yojson
   (>= 1.7.0))
  (core
   (>= v0.14.1))
  (ppx_yojson_conv
   (>= v0.14.0))
  (ppx_deriving
   (>= 5.2.1))))
