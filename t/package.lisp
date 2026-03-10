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
  (let ((obj1 (fj:jso "foo" 42 "bar" 32))
        (obj2 (fj:jso "foo" 42))
        (obj3 (fj:jso)))
    (is (equal '(("foo" . 42) ("bar" . 32)) (fj:collect #'cons obj1)))
    (is (equal '(("foo" . 42)) (fj:collect #'cons obj2)))
    (is (equal '() (fj:collect #'cons obj3)))))


(test jso-keys
  (let ((obj1 (fj:o "foo" 42 "bar" 32))
        (obj2 (fj:o :foo 42))
        (obj3 (fj:jso)))
    (is (equal '("foo" "bar") (fj:jso-keys obj1)))
    (is (equal '("foo") (fj:jso-keys obj2)))
    (is (equal '() (fj:jso-keys obj3)))))

(test jso-values
  (let ((obj1 (fj:jso "foo" 42 "bar" 32))
        (obj2 (fj:jso "foo" 42))
        (obj3 (fj:jso)))
    (is (equal '(42 32) (fj:jso-values obj1)))
    (is (equal '(42) (fj:jso-values obj2)))
    (is (equal '() (fj:jso-values obj3)))))

(test key-count
  (is (= 2 (fj:key-count (fj:jso "foo" 42 "bar" 32))))
  (is (= 1 (fj:key-count (fj:jso "foo" (fj:jso "bar" 32
                                              "baz" 12)))))
  (is (= 3 (fj:key-count (fj:jso "foo" (fj:jso "bar" 32
                                              "baz" 12)
                                "bar" 34
                                "baz" :false)))))
(test jsoλ
  (let ((func (jsoλ "foo"))
        (obj1 (fj:jso "foo" 42 "bar" 32))
        (obj2 (fj:jso "foo" 42)))
    (is (= 42 (funcall func obj1)))
    (is (= 42 (funcall func obj2)))))

(test with-keys
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
  (let* ((fj:*decode-lists-as* :array)
         (array (fj:read-json "[3,4,5,4]")))
    (is (typep array '(simple-vector 4)))
    (loop :for val :across #(3 4 5 4)
          :for i :from 0
          :do
             (is (= val (aref array i))))))

(test comments
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
    (is (string= "//trouble" (at obj :strange)))
    )

  )
