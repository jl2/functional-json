(in-package :functional-json)

;; Disabled while debugging
;; TODO: maybe permanently and allow user to set at load time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize*
    '(optimize (speed 3) (safety 1) (space 0) (debug 1) (compilation-speed 0)))
  (defparameter *fail-on-extra-keys* nil
    "If non-nil then v, vλ, and related functions will fail if there are extra
 keys when it gets to a non-jso object.  Otherwise the remaining keys are return
 in a list.")

  (defparameter *decode-objects-as* :jso
    "Valid values: :jso :hashtable
  Controls how js objects should be decoded. :jso means decode to internal struct which
  can be processed by getjso, mapjso etc. :hashtable means decode as hash tables.")

  (defparameter *decode-lists-as* :list
    "Valid values: :list :array
  Controls how js arrays should be decoded.
  :list means decode into a list.
  :array means decode into an array")

  (defparameter *allow-comments* nil
    "Non-nil means ignore C++ style // comments when parsing.")


  (defvar *reading-slot-name* nil
    "Used by read-json-atom to determine whether it should treat bare words as atoms or errors.
It is set to t by read-json-object in order to read bareword object keys such
 as { foo: 42} (as opposed to { \"foo\": 42 }) ")
  )




(define-condition json-error (simple-error) ()
  (:documentation "Generic JSON error."))
(define-condition json-index-error (json-error) ()
  (:documentation "Error condition raised when a function tries to reference too deeply into a JSON object."))
(define-condition json-parse-error (json-error) ()
  (:documentation "Unexpected or invalid syntax while parsing JSON."))
(define-condition json-eof-error (json-parse-error) ()
  (:documentation "Unexpected end of input when parsing JSON."))
(define-condition json-write-error (json-error) ()
  (:documentation "write-json-element called on an unsupported type.  Implement json-write-error for the type to fix."))
(define-condition json-type-error (json-error) ()
  (:documentation "Raised by read-json-as-type if the read JSON doesn't conform to the specified type."))

(defun raise (type format &rest args)
  "Convenience wrapper around error."
  (error type :format-control format :format-arguments args))


;; Types that might be useful when checking the type of input.
(deftype json-bool () '(member :true :false)
  "A Common Lisp representation of a JSON boolean value.")

(deftype json-null () '(eql :null)
  "A Common Lisp representation of a JSON null value.  This is :null, different than nil the empty list [].")

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
  "Return strings as they are but convert symbols and keywords to lower case strings.
\"foo\" -> \"foo\"
:foo -> \"foo\"
'foo -> \"foo\""
  (declare #.*optimize*)
  (typecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))
    (t key)))

(defun o (&rest fields)
  "Create a JSON object using symbols. Arguments should be alternating keys (strings or symbols) and values.  Like @'jso but calls #'key-to-string on the keys.
This is the preferred way to create JSON objects:
(o :foo 32
   :bar 16
   :nested (o \"omg\" (o :key 42)
              :key 23)) ->
{
  \"foo\": 32,
  \"bar\": 16,
  \"nested\": {
    \"omg\": {
      \"key\": 42
    },
    \"key\": 23
  }
}"
  (declare #.*optimize*)
  (make-jso :alist (loop :for (key val) :on fields :by #'cddr
                         :collect (cons (key-to-string key) val))))


(defmacro key-to-string-m (key)
  "A macro version of key-to-string.
Return strings as they are but convert symbols and keywords to lower case strings.
\"foo\" -> \"foo\"
:foo -> \"foo\"
'foo -> \"foo\""
  (declare #.*optimize*)
  `(typecase ,key
     (string ,key)
     (symbol (string-downcase (symbol-name ,key)))
     (t ,key)))

(defun jso (&rest fields)
  "Create a JSON object. Arguments should be alternating labels and values."
  (declare #.*optimize*)
  (make-jso :alist (loop :for (key val) :on fields :by #'cddr
                         :collect (cons key val))))

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

(defmacro getjso* (key-str jso)
  "Parse a sequence of . separated keys out of key-str and expand into nested calls to getjso.
(getjso* \"foo.bar\" obj) -> (getjso \"bar\" (getjso \"foo\" jso))"
  (declare #.*optimize*
           (type string key-str)
           (type symbol jso))
  (let ((last (position #\. key-str :from-end t)))
    (if last
        `(getjso ,(subseq key-str (1+ last))
                 (getjso* ,(subseq key-str 0 last) ,jso))
        `(getjso ,key-str ,jso))))
