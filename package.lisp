;; package.lisp
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

(defpackage :functional-json
  (:use :common-lisp)
  (:nicknames :fj)
  (:export #:o
           #:jso
           #:make-jso
           #:make-nested-object


           #:at
           #:at*
           #:at-list
           #:atλ
           #:atλ*
           #:with-keys
           #:getjso
           #:getjso*
           #:getjso+
           
           #:jsoλ

           #:collect
           #:mapjso

           #:copy-jso

           #:json-bool
           #:json-null


           #:jso-alist

           #:jso-from-alist

           #:jso-p
           #:key-count
           #:jso-keys
           #:jso-values

           #:json-error
           #:json-eof-error
           #:json-parse-error
           #:json-type-error


           #:as-json-bool
           #:from-json-bool

           #:print-indent
           #:print-json-element
           #:write-json
           #:write-json-element
           #:write-json-to-string

           #:read-json
           #:read-json-as-type
           #:read-json-atom
           #:read-json-element
           #:read-json-from-string
           #:read-json-list
           #:read-json-object
           #:read-json-string

           #:*allow-comments*
           #:*decode-objects-as*
           #:*output-literal-unicode*
           #:*pretty-print-indent-size*
           #:*print-object-style*
           #:*script-tag-hack*
           ))

