(in-package :functional-json)

;; Disabled while debugging
;; TODO: maybe permanently and allow user to set at load time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize*
    '(optimize (speed 3) (safety 1) (space 0) (debug 1) (compilation-speed 0))))


;; Types that might be useful when checking the type of input.
(deftype json-bool () '(member :true :false))
(deftype json-null () '(eql :null))

;; These are used to represent JS objects on the Lisp side -- hash
;; tables are too heavyweight on some implementations.
;; TODO: Can this use alists *or* hashtables
(defstruct jso
  alist)

(declaim (inline key-to-string
                 jso o
                 as-json-bool from-json-bool
                 getjso (setf getjso)
                 mapjso
                 make-jso))

(defun key-to-string (key)
  "Strings stay the same, but symbols are converted to to lower case strings"
  (declare #.*optimize*)
  (typecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))
    (t key)))

(defmacro key-to-string-m (key)
  "Strings stay the same, but symbols are converted to to lower case strings"
  `(typecase ,key
    (string ,key)
    (symbol (string-downcase (symbol-name ,key)))
    (t ,key)))

(defun jso (&rest fields)
  "Create a JS object. Arguments should be alternating labels and values."
  (declare #.*optimize*)
  (make-jso :alist (loop :for (key val) :on fields :by #'cddr
                         :collect (cons key val))))

(defun o (&rest fields)
  "Create a JS object. Arguments should be alternating labels and values. "
  (declare #.*optimize*)
  (make-jso :alist (loop :for (key val) :on fields :by #'cddr
                         :collect (cons (key-to-string key) val))))

;; Boolean types. It is hard to see what is meant by NIL when encoding
;; a lisp value -- false or [] -- so :false and :true are used instead
;; of T and NIL.
(defun as-json-bool (value)
  "Convert a generalised boolean to a :true/:false keyword."
  (declare #.*optimize*)
  (if value :true :false))

(defun from-json-bool (value)
  "Convert :true or :false to its boolean equivalent."
  (declare #.*optimize*)
  (ecase value (:true t) (:false nil)))


;; A hash-table-like interface for JS objects.
(defun getjso (key obj)
  "Return the value stored at key in obj, or nil.  Returns a second value of
t or nil indicating if the value existed in the object.
Unlike `at`, key must be a string."
  (declare #.*optimize*)
  (let ((pair (assoc key (jso-alist obj) :test #'string=)))
    (values (cdr pair) (and pair t))))

(defun (setf getjso) (val key map)
  "Store a value in a JS object."
  (declare #.*optimize*)
  (let ((pair (assoc key (jso-alist map) :test #'string=)))
    (if pair
        (setf (cdr pair) val)
        (prog1 val
          (push (cons key val)
                (jso-alist map))))))

(defun mapjso (func obj)
  "Call (func key value) for each key/value pair in obj."
  (declare #.*optimize*
           (type (function (t t) t) func))
  (loop :for (key . val) :in (jso-alist obj)
        :do (funcall func key val)))

(defun collect (func obj)
  "Call (func key value) for each key/value pair in obj and collect the results in a list.
Like mapjso, but collects the values returned by func."
  (declare #.*optimize*)
  (declare (type jso obj)
           (type (function (t t) t) func))
  (loop :for (key . val) :in (jso-alist obj)
        :collect (funcall func key val)))

;; (fj:getjso* "foo.bar" (fj:o :foo (fj:o :bar 32 :baz 2) :bar 1))
(defmacro getjso* (keys jso)
  "Like getjso, but splits keys on #\. to recursively access nested objects."
  (let ((split (loop
                 :for idx = 0 :then (1+ next-dot)
                 :for next-dot = (position #\. keys :start (1+ idx))
                 :collecting (subseq keys idx next-dot)
                 :while next-dot)))
    `(at* ,jso ,@split)))
