
(in-package :functional-json)

;; Utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fail-on-extra-keys* nil
    "If non-nil then v, vλ, and related functions will fail if there are extra keys when
 it gets to a non-jso object.  Otherwise the remaining keys are return in a list. "))

(defun make-nested-object (value keys)
  "(make-nested-object 42 '(:foo :bar :wat)) -> { foo: { bar: { wat: 42 } } }"
  (loop :for json = value
          :then (jso key-string json)
        :for key :in (reverse keys)
        :for key-string = (key-to-string key)
        :finally (return json)))

(defun jsoλ (&rest keys)
  "Return a function that will extract the given keys from a JSON object."
  (lambda (jso) (apply #'at jso keys)))


(defun at-list (jso keys)
  "Implementation for at - return a nested JSON value"

  (loop
    :for it = jso :then (getjso (key-to-string key) it)
    :while (eq (type-of it) 'jso)
    :for (key . remainder) :on keys
    :finally
       (return
         (if (and remainder *fail-on-extra-keys*)
             (error "Too many keys in js:v")
             (values it remainder))))

  ;; Recursive implementation:
  ;; (cond
  ;;   ((and (eq (type-of jso) 'jso)
  ;;         keys
  ;;         (cdr keys))
  ;;    (at-list (getjso (key-to-string (car keys)) jso)
  ;;             (cdr keys)))
  ;;   ((and (eq (type-of jso) 'jso)
  ;;          keys)
  ;;     (getjso (key-to-string (car keys)) jso))
    
  ;;   ((null keys)
  ;;    jso)
  ;;   ((and keys
  ;;         (not *fail-on-extra-keys*))
  ;;    (values jso keys))
  ;;   (t
  ;;    (error "Too many keys in at-list")))
)

(defun (setf at-list) (val jso keys)
  "Implementation for (setf at) - set a nested JSON value"
  (let* ((key (car keys))
         (key-string (key-to-string key))
         (alist (assoc key-string
                       (jso-alist jso)
                       :test #'string=)))
    (cond
      ((and alist
            (null (cdr keys)))
       (setf (cdr alist) val))
      
      ((and alist
            (cdr keys))
       (setf (at-list (cdr alist) (cdr keys)) val))
      ((and (null alist) (null keys))
       (prog1 val (push (cons key-string val)
                        (jso-alist jso))))
      ((and (null alist))
       (prog1 val (push (cons key-string (make-nested-object val (cdr keys)))
                        (jso-alist jso)))))))

(defun at (jso &rest keys)
  "Return nested JSON values:
(let* ((json (js:read-json \"
{\"a\": {
   \"b\": {
     \"c\": {
       \"d\": {
         \"e\": 42
}}}}}\")))
  (js:at json :a :b :c \"d\" :e)
42"
  (at-list jso keys))

(defmacro at* (jso &rest keys)
  "Macro version of v"
  (loop
    :for so-far = jso :then this
    :for key :in keys
    :for this = `(getjso (key-to-string-m ,key) ,so-far)
    :finally (return this)))

(defun (setf at) (val jso &rest keys)
  "Return nested JSON values:
(let* ((json (js:read-json \"
{\"a\": {
   \"b\": {
     \"c\": {
       \"d\": {
         \"e\": 42

}}}}}\")))
  (setf (js:at json :a :d :c \"d\" :e) 12)
12"
  (declare (type jso jso))
  (setf (at-list jso keys) val))


(defun atλ (jso &rest keys)
  "Curry #'at creating a function that takes keys and returns values:
(let* ((json (js:read-json \"
{\"a\": {
   \"b\": {
     \"c\": {
       \"d\": {
         \"e\": 42
}}}}}\"))
       (temp (js:vλ :a :b :c)))
    (funcall temp :d :e))
42"
  (lambda (&rest more-keys)
    (apply #'at jso (concatenate 'list
                                keys
                                more-keys))))
(defun (setf atλ) (val jso &rest keys)
  "Curry #'at creating a function that takes keys and returns values:
(let* ((json (js:read-json \"
{\"a\": {
   \"b\": {
     \"c\": {
       \"d\": {
         \"e\": 42
}}}}}\"))
       (temp (js:vλ :a :b :c)))
    (funcall temp :d :e))
42"
  (lambda (&rest more-keys)
    (setf (apply #'at jso (concatenate 'list
                                       keys
                                       more-keys))
          val)))


(defmacro atλ* (jso &rest keys)
  "Macro version of vλ"
  (let ((inner (loop
                 :for so-far = jso :then this
                 :for key :in keys
                 :for this = (list 'js:getjso `(key-to-string-m ,key) so-far)
                 :finally (return this))))
    `(let ((inval ,inner))
       (lambda (&rest more-keys)
         (apply #'v inval more-keys)))))

(defun collect (func obj)
  "Like mapjso, but returning the results.
   Returns a list with the results of calling func on each key/value pair in a JS object."
  (declare (type jso obj)
           (type (function (t t) t) func))
  (loop :for (key . val) :in (jso-alist obj)
        :collect (funcall func key val)))

(defun jso-keys (obj)
  "Return the keys from obj."
  (loop :for (key . val) :in (jso-alist obj)
        :collecting key))

(defun jso-values (obj)
  "Return the values from obj."
  (loop :for (key . val) :in (jso-alist obj)
        :collecting val))

(defun jso-from-alist (vals)
  "Create a jso directly from an alist."
  (make-jso :alist vals))

(defun getjso+ (key jso)
  "key is a string of the form \"key1.key2. ... key99\" and returns the value:
    (getjso \"key3\"
      (getjso \"key2\"
        (getjso \"key1\" jso)))
   This is like getjso*, but as a function."
  (declare (type string key)
           (type jso jso))
  (let* ((last (position #\. key :from-end t))
         (inner (when last (getjso+ (subseq key 0 last) jso))))
    
    (cond
      ((and last
            inner
            (eql 'jso (type-of inner)))
       (getjso (subseq key (1+ last)) inner))
      (t
       (getjso key jso)))))


(defun key-count (obj)
  "The number of key/value pairs in obj"
  (typecase obj
    (list (length obj))
    (jso
     (length (jso-alist obj)))
    (hash-table
     (hash-table-size obj))
    (t 1)))

(defmacro with-keys (key-names obj
                     &body body)
  "Convenient access to nested JSON elements.
(with-keys ((a :key1)
               (b :key2 :key3)) some-json)
     ;; a and b pointing into some-json
     ;; and can be used with setf, rotatef, etc.
     )"
  (let ((el (gensym)))
    `(let ((,el ,obj))
       (declare (ignorable ,el))
       (symbol-macrolet
           ,(mapcar (lambda (kname)
                      `(,(car kname)
                        (at ,el ,@(cdr kname))))
             key-names)
         ,@body))))

