(in-package :cl-user)

(defpackage :functional-json.test
  (:use :cl
   :fiveam
        :alexandria
   :functional-json))

(in-package :functional-json.test)

(def-suite :functional-json)
(in-suite :functional-json)

(test collect
  ;; Test running a function on key/values and collecting to a list
  (let ((obj1 (fj:jso "foo" 42 "bar" 32))
        (obj2 (fj:jso "foo" 42))
        (obj3 (fj:jso)))
    (is (equal '(("foo" . 42) ("bar" . 32)) (fj:collect #'cons obj1)))
    (is (equal '(("foo" . 42)) (fj:collect #'cons obj2)))
    (is (equal '() (fj:collect #'cons obj3)))))


(test jso-keys
  ;; Test getting JSON keys
  (let ((obj1 (fj:o "foo" 42 "bar" 32))
        (obj2 (fj:o :foo 42))
        (obj3 (fj:jso)))
    (is (equal '("foo" "bar") (fj:jso-keys obj1)))
    (is (equal '("foo") (fj:jso-keys obj2)))
    (is (equal '() (fj:jso-keys obj3)))))

(test jso-values
  ;; Test getting JSON values
  (let ((obj1 (fj:jso "foo" 42 "bar" 32))
        (obj2 (fj:jso "foo" 42))
        (obj3 (fj:jso)))
    (is (equal '(42 32) (fj:jso-values obj1)))
    (is (equal '(42) (fj:jso-values obj2)))
    (is (equal '() (fj:jso-values obj3)))))

(test key-count
  ;; Test key counting
  (is (= 2 (fj:key-count (fj:jso "foo" 42 "bar" 32))))
  (is (= 1 (fj:key-count (fj:jso "foo" (fj:jso "bar" 32
                                              "baz" 12)))))
  (is (= 3 (fj:key-count (fj:jso "foo" (fj:jso "bar" 32
                                              "baz" 12)
                                "bar" 34
                                "baz" :false)))))
(test jsoλ
  ;; Test creating an accessor function
  (let ((func (jsoλ "foo"))
        (obj1 (fj:jso "foo" 42 "bar" 32))
        (obj2 (fj:jso "foo" 42)))
    (is (= 42 (funcall func obj1)))
    (is (= 42 (funcall func obj2)))))

(test with-keys
  ;; Test accessing values using with-keys
  (let ((obj (fj:o :foo 34
                   :bar (fj:o :wat 12
                              :second 4)
                   :third (fj:o :a 1
                                :b 2
                                :c 3))))
    (fj:with-keys ((f :foo)
                   (a :third :a)
                   (b :third :b)
                   (c "third" "c")
                   (w "bar" :wat)) obj
      (is (= (+ f a b c  w)
             (+ 1 2 3 12 34))))
    (fj:with-keys ((f :foo)
                   (a :third :a)) obj
      (rotatef a f))

    (fj:with-keys ((f :foo)
                   (a :third :a)) obj
      (rotatef a f))
    (is (= (fj:getjso "foo" obj) 34))
    (is (= (fj:at obj :third :a) 1))))

(test print-json-element
  ;; Basic pretty printing tests
  (let ((fj::*print-object-style* :pretty)
        (obj1 (fj:jso "foo" 42 "bar" 32))
        (obj2 (fj:jso "foo" 42))
        (obj3 (fj:jso "foo" (fj:jso "bar" 42
                                    "foof" 32)
                      "bar" (fj:jso "bar" 12
                                    "wat" 34)
                      "wat" 99)))
    (is (string= "{
    \"foo\": 42,
    \"bar\": 32
}"
                 (with-output-to-string (outs)
                   (print-object obj1 outs))))
    (is (string= "{
    \"foo\": 42
}"
                 (with-output-to-string (outs)
                   (print-object obj2 outs))))
    (is (string= "{
    \"foo\": {
        \"bar\": 42,
        \"foof\": 32
    },
    \"bar\": {
        \"bar\": 12,
        \"wat\": 34
    },
    \"wat\": 99
}"
                 (with-output-to-string (outs)
                   (print-object obj3 outs))))

    ))

(test array-reading
  ;; Test reading  lists into vectors
  (let* ((fj:*decode-lists-as* :array)
         (array (fj:read-json "[3,4,5,4]")))
    (is (typep array '(simple-vector 4)))
    (loop :for val :across #(3 4 5 4)
          :for i :from 0
          :do
             (is (= val (aref array i))))))

(test comments
  ;; Test that the reader skips comments
  (let* ((fj:*allow-comments* t)
         (obj (fj:read-json "{ // testing
\"foo\": 34, \"what\": // skip it
42
}")))
    (is (= 34 (at obj :foo)))
    (is (= 42 (at obj :what)))
    )
  (let* ((fj:*allow-comments* t)
         (fj:*decode-lists-as* :array)
         (obj (fj:read-json "
// This is // a comment //
{ // testing another comment
\"foo\": 34, \"what\": // skip it
42,
\"bar\": [5 // comment
, // stand alone comma!
5,3,// uh-oh
4],
\"strange\": \"//trouble\"
}")))
    (is (= 34 (at obj :foo)))
    (is (= 42 (at obj :what)))
    (is (= 5 (at obj :bar 0)))
    (is (= 5 (at obj :bar 1)))
    (is (= 3 (at obj :bar 2)))
    (is (= 4 (at obj :bar 3)))
    (is (string= "//trouble" (at obj :strange)))))

;; Declare some JSON types for the next text.
;; If these aren't at  the top-level then the compiler will warn about undefined functions at compile time,
;; but the types will be defined correctly at runtime.  Declaring at the top level avoids the problem.

(eval-when
    (:compile-toplevel :load-toplevel :execute)
  (fj:def-jso-type automobile
      ((:manufacturer :name)
       (:manufacturer :country)
       :model
       :engine))
  ;; An ice automobile has an engine with a cylinder count and displacement
  (fj:def-jso-type ice-automobile
      ((:engine :cylinder-count)
       (:engine :displacement)))
  ;; An EV automobile has an engine with watts and volts
  (fj:def-jso-type ev-automobile
      ((:engine :watts)
       (:engine :volts))))

(test typing
  
  (flet ((what-is-it (car)
           ;; Check JSON types using typecase
           (typecase car
             (ev-automobile "Electric car")
             (ice-automobile "Combustion car")
             (automobile "Just a car"))))

    ;; Check JSON types using typep
    (let ((bmw (fj:read-json "
    {
      \"manufacturer\": {
         \"name\": \"BMW\",
         \"country\": \"Germany\"
      },
      \"model\": \"M3\",
      \"engine\": {
         \"cylinder-count\": 6,
         \"displacement\": 6.0
      }
    }"))
          (tesla (fj:read-json "
  {
    \"manufacturer\": {
       \"name\": \"Tesla\",
       \"country\": \"usa\"
    },
    \"model\": \"Whatever\",
    \"engine\": {
       \"watts\": 6,
       \"volts\": 120.0
    }
  }")))
      (is (typep bmw 'automobile))
      (is (typep bmw 'ice-automobile))
      (is (not (typep bmw 'ev-automobile)))
      (is (string=  "Combustion car" (what-is-it bmw)))
      (is (typep tesla 'automobile))
      (is (not (typep tesla 'ice-automobile)))
      (is (typep tesla 'ev-automobile))
      (is (string= "Electric car" (what-is-it tesla))))))


(test indexing
  (let ((bmw (fj:read-json "
    {
      \"manufacturer\": {
         \"name\": \"BMW\",
         \"country\": \"Germany\"
      },
      \"model\": \"M3\",
      \"engine\": {
         \"cylinder-count\": 6,
         \"displacement\": 6.0
      }
    }"))
        (tesla (fj:read-json "
  {
    \"manufacturer\": {
       \"name\": \"Tesla\",
       \"country\": \"usa\"
    },
    \"model\": \"Whatever\",
    \"engine\": {
       \"watts\": 6,
       \"volts\": 120.0
    }
  }")))
    (is (string= "BMW" (fj:at bmw "manufacturer" "name")))
    (is (string= "BMW" (fj:at bmw :manufacturer :name)))
    (is (string= "BMW" (fj:at bmw "manufacturer" :name)))
    (is (string= "BMW" (fj:at-list bmw '("manufacturer" :name))))
    (is (= (funcall (fj:jsoλ :engine :watts) tesla) 6))
    (is (= 120.0 (fj:getjso* "engine.volts" tesla)))
    ))


(test unicode
  (let ((obj (fj:o :foo (fj:read-json "\"15\\u00b0C\""))))
    (string= (fj:at obj :foo) "15°C"))

  ;; TODO: Why does this fail on ABCL?
  (let* ((expected-string "In JSON, 𝄞 can be encoded/escaped like this: 𝄞")
         (json-string "\"In JSON, 𝄞 can be encoded/escaped like this: \\uD834\\uDD1E\"")
         (result (fj:read-json json-string)))
    (is (string= result expected-string)))
  )
