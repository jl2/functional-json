(in-package :functional-json)

;; Utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fail-on-extra-keys* nil
    "If non-nil then v, vλ, and related functions will fail if there are extra
 keys when it gets to a non-jso object.  Otherwise the remaining keys are return
 in a list."))

(declaim (inline next-thing))
(defun next-thing (json idx)
  (declare (optimize (speed 3)))
  (let ((xformed (key-to-string idx)))
    (etypecase xformed
      (string (getjso xformed json))
      (integer (if (< idx (length json))
                   (values (nth xformed json) t)
                   (values  nil nil)))
      (list (at-list json xformed)))))

(defun make-nested-object (value keys)
  "(make-nested-object 42 '(:foo :bar :wat)) -> { foo: { bar: { wat: 42 } } }"
  (loop
    :for json = value :then (jso key-string json)
    :for key :in (reverse keys)
    :while key
    :for key-string = (key-to-string key)
    :finally (return json)))

(defun jsoλ (&rest keys)
  "Return a function that will extract the given keys from a JSON object."
  (lambda (jso) (at-list jso keys)))


(defun at-list (jso keys)
  "Implementation for at - return a nested JSON value"
  (loop
    :with val = jso
    :with present = t
    :for (key . remainder) :on keys
    :while (and present
                key)
    :do
       (setf (values val present) (next-thing val key))
    :finally
       (return
         (values val present)))

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
  "Implementation for (setf at) - set a nested JSON value.  setf into a list is
not supported, only string and symbol keys are supported."
  (let* ((key (car keys))
         (key-string (key-to-string key))
         (alist-entry (assoc key-string
                       (jso-alist jso)
                       :test #'string=)))
    ;; This doesn't support setf into a list or array.
    (cond
      ((and alist-entry
            (null (cdr keys)))
       (setf (cdr alist-entry) val))

      ((and alist-entry
            (cdr keys))
       (setf (at-list (cdr alist-entry) (cdr keys)) val))
      ((and (null alist-entry) (null keys)
            (null val))

       (prog1 val (push (cons key-string val)
                        (jso-alist jso))))
      ((and (null alist-entry))
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
    :for real-key = (key-to-string-m key)
    :for this = `(getjso ,real-key ,so-far)
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
(defmacro atλ* (jso &rest keys)
  "Macro version of atλ"
  (let ((inner (loop
                 :for so-far = jso :then this
                 :for key :in keys
                 :for this = (list 'getjso `(key-to-string-m ,key) so-far)
                 :finally (return this))))
    `(let ((inval ,@inner))
       (lambda (&rest more-keys)
         (apply #'at inval more-keys)))))

(defun jso-keys (obj)
  "Return the keys from obj."
  (loop :for (key . nil) :in (jso-alist obj)
        :collecting key))

(defun jso-values (obj)
  "Return the values from obj."
  (loop :for (nil . val) :in (jso-alist obj)
        :collecting val))

(defun jso-from-alist (vals)
  "Create a jso directly from an alist."
  (make-jso :alist vals))

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

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by
LIST."
  (if (listp list)
      list
      (list list)))

(defun mapping (&rest mappings)
  (loop :for (first second) :on mappings :by #'cddr
                :collecting
                (cons (ensure-list first) (ensure-list second))))

(defun transformer (&rest mappings)
  ;;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((sets (loop
                 :for (from to) :on mappings :by #'cddr
                 :for froml = (ensure-list from)
                 :for tol = (ensure-list to)
                 :collecting `(setf (at rval ,@tol)
                                    (at* obj ,@froml)))))

    (eval `(lambda (obj)
             (declare (optimize (speed 3) (safety 0) (debug 0)))
             (let ((rval (jso)))
               ,@sets
               rval)))))

(defun transform (xform obj)
  (loop
    :with rval = (jso)
    :for (old-keys . new-keys) :in xform
    :do
       (format t "Moving ~a (~a) to ~a~%"

               old-keys (at-list obj old-keys)
               new-keys)
       (setf (at-list rval new-keys)
             (at-list obj old-keys))

    :finally (return rval)))

(defmacro def-jso-type (type-name keys)
  "Define function #'<type-name>-p and type <type-name> to represent JSON
objects with specified keys."
  (let ((is-name (intern (string-upcase (format nil "~a-p" type-name))))
        (key-var (gensym "keys")))
    `(progn
       (defun ,is-name (val)
         (let ((,key-var ,keys))
           (declare (type jso val))
           (loop :with has-key = t
                 :with ignored = nil
                 :while has-key
                 :for key in ,key-var
                 :do (setf (values ignored has-key) (at-list val key))
                 :finally (return has-key))))
       (deftype ,type-name ()
         '(and jso
           (satisfies ,is-name))))))
