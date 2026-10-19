;; package.lisp
;; Copyright (c) 2025 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage :functional-json
  (:use :common-lisp :alexandria)
  (:nicknames :fj)
  (:export #:o
         #:jso
         #:make-jso
         #:make-nested-object
         #:key-to-string

         #:mapping
         #:transform
         #:transformer

         #:show

         #:at
         #:at*
         #:at-list

         #:del
         #:del-list

         #:atλ
         #:atλ*
         #:with-keys
         #:getjso
         #:getjso*

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

         #:apply-patch
         #:parse-patch-path
         #:create-custom-op
         #:*ops*
         #:json-equal
         #:deep-copy

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

         #:read-json-list
         #:read-json-object
         #:read-json-string

         #:def-jso-type

         #:*allow-comments*
         #:*decode-objects-as*
         #:*decode-lists-as*
         #:*output-literal-unicode*
         #:*pretty-print-indent-size*
         #:*print-object-style*
         #:*script-tag-hack*

         ))
