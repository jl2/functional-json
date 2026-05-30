(in-package :functional-json)

;; Utilities

(declaim (inline next-thing))
(defun next-thing (json idx)
  "Return json[idx] if json is a list or array, errors if json isn't a list, vector, or json object, or if idx isn't a string, integer, or list of keys."
  (declare (optimize (speed 1))
           (type (or list vector jso) json))
  (let ((xformed (key-to-string idx)))
    (etypecase xformed
      (string (getjso xformed json))
      (integer
       (if (< idx (length json))
           (etypecase json
             (list (values (nth xformed json) t))
             (array (values (aref json xformed) t)))
           (values  nil nil)))
      (list (at-list json xformed)))))

(declaim (inline  jsoλ at jso-from-alist (setf at)))
(declaim (notinline at-list (setf at-list)))

(defun at-list (jso keys)
  "Implementation for at - return a nested JSON value"
  (loop

    :with current-jso = jso
    :with present = t

    :for (key . remainder) :on (ensure-list keys)
    :while (and present key current-jso)
    :do
       (setf (values current-jso present) (next-thing current-jso key))

    :when (and (not present)
           remainder
           *fail-on-extra-keys*)
      :do
         (raise 'json-index-error "Too many keys in at-list for object ~a and keys ~a" jso keys)
    :finally
       (return (values current-jso present)))

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
  ;;    (raise "Too many keys in at-list")))
  )

(defun del-list (jso keys)
  "Implementation for del - delete a nested JSON value"
  (loop
    :with current-jso = jso
    :with present = t
    :for (key . remainder) :on (ensure-list keys)
    :while (and present key)

    :when remainder
      :do
         (setf (values current-jso present) (next-thing current-jso key))

    :when (and (not present)
               remainder
               *fail-on-extra-keys*)
      :do
         (raise 'json-index-error "Too many keys in at-list for object ~a and keys ~a" jso keys)

    :when (and present
               (null remainder))
      :do
         (setf (jso-alist current-jso)
               (remove (key-to-string key)
                       (jso-alist current-jso)
                       :test #'string=
                       :key #'car))
    :finally
       (return
         (values jso present))))

(defun (setf at-list) (val jso keys)
  "Implementation for (setf at) - set a nested JSON value.  setf into a list is
not supported, only string and symbol keys are supported."
  (declare #.*optimize*)
  (let* ((lkeys (ensure-list keys))
         (key (car lkeys))
         (key-string (key-to-string key))
         (alist-entry (assoc key-string
                             (jso-alist jso)
                             :test #'string=)))
    ;; This doesn't support setf into a list or array.
    (cond
      ((and alist-entry
            (null (cdr lkeys)))
       (setf (cdr alist-entry) val))

      ((and alist-entry
            (cdr lkeys))
       (setf (at-list (cdr alist-entry) (cdr lkeys)) val))

      ((and (null alist-entry) (null lkeys)
            (null val))

       (prog1 val (push (cons key-string val)
                        (jso-alist jso))))
      ((and (null alist-entry))
       (prog1 val (push (cons key-string (make-nested-object val (cdr lkeys)))
                        (jso-alist jso)))))))

(defun make-nested-object (value keys)
  "(make-nested-object 42 '(:foo :bar :wat)) -> { foo: { bar: { wat: 42 } } }"
  (declare #.*optimize*)
  (loop
    :for json = value :then (jso (key-to-string key) json)
    :for key :in (reverse keys)
    :while key
    :finally (return json)))


(defun jsoλ (&rest keys)
  "Return a function that will extract the given keys from a JSON object."
  (declare #.*optimize*)
  (lambda (jso) (at-list jso keys)))

(declaim (inline at))
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

(defun del (jso &rest keys)
  "Remove element specified by keys from jso
(let* ((json (js:read-json \"
{\"a\": {
   \"b\": {
     \"c\": {
       \"d\": {
         \"e\": 42
}}}}}\")))
  (js:at json :a :b :c \"d\" :e)
42"

  (del-list jso keys))

(defmacro at* (jso &rest keys)
  "Macro version of at"
  `(loop
     :for so-far = ,jso :then this
     :for key :in (quote ,keys)
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
  (declare #.*optimize*)
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
  (declare #.*optimize*)
  (lambda (&rest more-keys)
    (declare #.*optimize*)
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
       (declare #.*optimize*)
       (lambda (&rest more-keys)
         (apply #'at inval more-keys)))))

(declaim (inline jso-keys jso-values jso-from-alist key-count))
(defun jso-keys (obj)
  "Return the keys from obj."
  (declare #.*optimize*)
  (loop :for (key . nil) :in (jso-alist obj)
        :collecting key))


(defun jso-values (obj)
  "Return the values from obj."
  (declare #.*optimize*)
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

(defun mapping (&rest mappings)
  (declare #.*optimize*)
  (loop :for (first second) :on mappings :by #'cddr
        :collecting
        (cons (ensure-list first) (ensure-list second))))

(defmacro deftransformer (from to)
  (declare #.*optimize*)
  (let ((sets (loop
                :for old-key :in from
                :for new-key :in to
                :collecting `(setf (at-list rval ,new-key)
                                   (at-list obj ,old-key)))))

    `(lambda (obj)
       (declare #.*optimize*)
       (let ((rval (jso)))
         ,@sets
         rval))))



(defun transform (obj from to)
  "Create a new JSON object where the keys in `from` are moved to corresponding new keys in the new object.
(transform
   {
      \"foo\": {
         \"bar\": 42,
         \"foo\": 67
      },
      \"test\": {
        \"abc\": 1},
        \"lame\": 13
      }
   }
   '((:foo :bar) :test)
   '(:bar '(:nest :test))
   )
->
{
  \"bar\": 42,
  \"nest\": {
    \"test\": {
      \"abc\": 1
   }
 }
}
"
  (loop
    :with rval = (jso)
    :for old-key :in from
    :for new-key :in to
    :do
       (setf (at-list rval (ensure-list new-key))
             (at-list obj (ensure-list old-key)))

    :finally (return rval)))

(defmacro def-jso-type (type-name keys)
  "Define function #'<type-name>-p and type <type-name> to represent JSON
objects with specified keys."
  (declare #.*optimize*)
  (let ((is-name (intern (string-upcase (format nil "~a-p" type-name))))
        (key-var (gensym "keys")))
    `(progn
       (defun ,is-name (val)
         (let ((,key-var (quote  (,@keys))))
           (declare (type jso val))
           (loop :with has-key = t
                 :while has-key
                 :for key :in ,key-var
                 :do (multiple-value-bind (val present) (at-list val (ensure-list key))
                       (declare (ignorable val))
                       (setf has-key present))
                 :finally (return has-key))))
       (deftype ,type-name ()
         '(and jso
           (satisfies ,is-name))))))

(defun read-json-as-type (source type)
  "Read a JSON value and assert the result to be of a given type.
The type should have been defined with def-jso-type.
Raises a json-type-error when the type is wrong."
  (declare (optimize (speed 1) (safety 1) (space 0) (debug 1) (compilation-speed 0))
           (type symbol type))
  (let ((val (read-json source)))
    (if (typep val type)
        val
        (raise 'json-type-error "JSON input '~A' is not of expected type ~A." source type))))

(defgeneric deep-copy (a)
  (:documentation "Make a deep copy of the value a.
Sequences create new sequences whose contents are deep copied.
Hash-tables and jso objects are new objects who's values are deep-copied.
"))

(defmethod deep-copy ((a jso))
  (make-jso :alist
                   (loop :for (a . b) :in (jso-alist a)
                         :collecting (cons a (deep-copy b)))))


(defmethod deep-copy ((a sequence))
  (map (type-of a) #'deep-copy a))

(defmethod deep-copy ((a hash-table))
  (let ((the-copy (make-hash-table :test (hash-table-test a)
                                   :size (hash-table-size a)
                                   :rehash-size (hash-table-rehash-size a)
                                   :rehash-threshold (hash-table-rehash-threshold a))))
    (loop
      :for key
        :being :the hash-keys
          :of a
            :using (hash-value value)
      :do
         (setf (gethash key the-copy) (deep-copy value)))
    the-copy))

(defmethod deep-copy (a)
  a)

(defgeneric json-equal (a b)
  (:documentation "Test if two objects are equal by the rules of JSON."))

(defmethod json-equal (a b)
  nil)

(defmethod json-equal ((a string) (b string))
  (string= a b))

(defmethod json-equal ((a number) (b number))
  (= a b))

(defmethod json-equal ((a symbol) (b symbol))
  (eq a b))

(defmethod json-equal ((a sequence) (b sequence))
  (and (= (length a) (length b))
       (loop :for a-val :in (coerce a 'list)
             :for b-val :in (coerce b 'list)
             :for equal = (json-equal a-val b-val)
             :while equal
             :finally (return equal))))


(defmethod json-equal ((a jso) (b jso))
  (let ((a-keys (jso-keys a))
        (b-keys (jso-keys b)))
    (and (= (length a-keys) (length b-keys))
         (loop :for a-key :in (sort a-keys #'string<)
               :for b-key :in (sort b-keys #'string<)
               :for equal = (and (string= a-key b-key)
                                 (json-equal (at a a-key)
                                             (at b a-key)))
               :while equal
               :finally (return equal)))))

(defmethod json-equal ((a hash-table) (b hash-table))
  (and (= (hash-table-count a)
          (hash-table-count b))
       (loop
         :with a-val = nil
         :with a-present = t
         :with b-val = nil
         :with b-present = t
         :with equals = t

         :for key
           :being :the hash-keys :of a
         :do
            (setf (values a-val a-present) (gethash key a))
            (setf (values b-val b-present) (gethash key b))
            (setf equals (and equals
                              a-present
                              b-present
                              (json-equal a-val b-val)))
         :while equals
         :finally (return equals))

       (loop
         :with a-val = nil
         :with a-present = t
         :with b-val = nil
         :with b-present = t
         :with equals = t

         :for key
           :being :the hash-keys :of b
         :do
            (setf (values a-val a-present) (gethash key a))
            (setf (values b-val b-present) (gethash key b))
            (setf equals (and equals
                              a-present
                              b-present
                              (json-equal a-val b-val)))
         :while equals
         :finally (return equals))))
