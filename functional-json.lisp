(in-package :functional-json)

;; Disabled while debugging
;; TODO: maybe permanently and allow user to set at load time
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defparameter *optimize*
;;     '(optimize (speed 1) (safety 3) (space 3) (debug 1) (compilation-speed 0))))


;; Types that might be useful when checking the type of input.
(deftype json-bool () '(member :true :false))
(deftype json-null () '(eql :null))

;; These are used to represent JS objects on the Lisp side -- hash
;; tables are too heavyweight on some implementations.
;; TODO: Can this use alists *or* hashtables
(defstruct jso
  alist)


(defun key-to-string (key)
  "Strings stay the same, but symbols are converted to to lower case strings"
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))))

(defmacro key-to-string-m (key)
  "Strings stay the same, but symbols are converted to to lower case strings"
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))))


(defun jso (&rest fields)
  "Create a JS object. Arguments should be alternating labels and values."
  (make-jso :alist (loop :for (key val) :on fields :by #'cddr
                         :collect (cons key val))))

(defun o (&rest fields)
  "Create a JS object. Arguments should be alternating labels and values."
  (make-jso :alist (loop :for (key val) :on fields :by #'cddr
                         :collect (cons (key-to-string key) val))))

;; Boolean types. It is hard to see what is meant by NIL when encoding
;; a lisp value -- false or [] -- so :false and :true are used instead
;; of T and NIL.
(defun as-json-bool (value)
  "Convert a generalised boolean to a :true/:false keyword."
  (if value :true :false))
(defun from-json-bool (value)
  "Convert :true or :false to its boolean equivalent."
  (ecase value (:true t) (:false nil)))


;; A hash-table-like interface for JS objects.
(defun getjso (key map)
  "Fetch a value from a JS object. Returns a second value like
gethash."
  (let ((pair (assoc key (jso-alist map) :test #'string=)))
    (values (cdr pair) (and pair t))))

(defun (setf getjso) (val key map)
  "Store a value in a JS object."
  (let ((pair (assoc key (jso-alist map) :test #'string=)))
    (if pair
        (setf (cdr pair) val)
        (prog1 val (push (cons key val) (jso-alist map))))))

(defun mapjso (func map)
  "Iterate over the key/value pairs in a JS object."
  (loop :for (key . val) :in (jso-alist map)
        :do (funcall func key val)))

(defmacro getjso* (keys jso)
  (let ((last (position #\. keys :from-end t)))
    (if last
        `(getjso ,(subseq keys (1+ last))
                 (getjso* ,(subseq keys 0 last) ,jso))
        `(getjso ,keys ,jso))))
