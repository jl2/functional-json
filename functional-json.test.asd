;; functional-json.test.asd
;; Copyright (c) 2025 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :cl-user)
(defpackage :functional-json.test-asd
  (:use :cl :asdf))
(in-package :functional-json.test-asd)

(asdf:defsystem #:functional-json.test
  :description "Test functional-json"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC"
  :version "0.0.1"
  :serial t

  :depends-on ( :functional-json
                :fiveam)
  
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op
            :after (op c)
            (eval
             (read-from-string
              "(every #'fiveam::TEST-PASSED-P
                     (5am:run :functional-json))"))))
