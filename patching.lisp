(in-package :functional-json)

;; RFC6902 JSON Patch operations


(defun parse-patch-path (path)
  "Parse JSON PATCH paths out of strings and into lists of keys suitable for #'at-list.
Converts a string of the form \"/key1/key2/key3\" into the list (\"key1\" \"key2\" \"key3\")"
  (declare (type string path))
  (loop
    :for (start end)
      :on (loop
            :for i :of-type fixnum :from 0
            :for char :of-type character :across path
            :when (char= #\/ char)
              :collect i)
    :by #'cdr
    :collecting (subseq path (1+ start) end)))

(def-jso-type patch-test-op (:op :path :value))
(def-jso-type patch-add-op (:op :path :value))
(def-jso-type patch-replace-op (:op :path :value))
(def-jso-type patch-move-op (:op :path :from))
(def-jso-type patch-copy-op (:op :path :from))
(def-jso-type patch-remove-op (:op :path))

(defparameter *ops*
  `(("test"
     .
     ,(lambda (obj args)
        (with-keys ((path :path)
                    (value :value)) args
          (values (json-equal (at-list obj (parse-patch-path path))
                              value)
                  obj))
        ))
    ("remove"
     .
     ,(lambda (obj args)
        (declare (type jso obj)
                 (type patch-remove-op args))
        (with-keys ((path :path)) args
          (del-list obj (parse-patch-path path))
          (values t obj))))

    ("add"
     .
     ,(lambda (obj args)
        (declare (type jso obj)
                 (type patch-add-op args))
        (with-keys ((path :path)
                    (value :value)) args
          (setf (at-list obj (parse-patch-path path)) value)
          (values t obj))))

    ("replace"
     .
     ,(lambda (obj args)
        (declare (type jso obj)
                 (type patch-replace-op args))
        (let* ((path (at args :path))
               (new-value (at args :value))
               (pp (parse-patch-path path)))
          (multiple-value-bind (value existed) (at-list obj pp)
            (declare (ignorable value))
            (cond ((not existed)
                   (values nil obj))
                  (t
                   (setf (at-list obj pp) new-value)
                   (values t obj)))))))

    ("move"
     .
     ,(lambda (obj args)
        (declare (type jso obj)
                 (type patch-move-op args))
        (let ((path (parse-patch-path (at args :path)))
              (from (parse-patch-path (at args :from))))

          (multiple-value-bind (value existed) (at-list obj from)
            (cond ((not existed)
                   (values nil obj))
                  (t
                   (setf (at-list obj path) value)
                   (del-list obj from)
                   (values t obj)))))))

    ("copy"
     .
     ,(lambda (obj args)
        (declare (type jso obj)
                 (type patch-copy-op args))
        (let* ((path (parse-patch-path (at args :path)))
               (from (parse-patch-path (at args :from))))
          (multiple-value-bind (value existed) (at-list obj from)
            (cond ((not existed)
                   (values nil obj))
                  (t
                   (setf (at-list obj path)
                         (deep-copy  value))
                   (values t obj))))))))

  "This is an association list of operations that are supported in the 'op' field of PATCH objects.
The entries are of the form (name . function) where name is the op's name as a string and the function
takes two arguments - the JSON object to apply the operation to, and an object containing all of the arguments.
Add values using #'create-custom-op")

(defun create-custom-op (name function)
  "Add a custom operation to the JSON PATCH processing.
Built-in operations are specified by JSON PATCH, and are:
    test, remove, add, replace, move, and copy."
  (push (cons name function) *ops*))


;; TODO: Decide the semantics
;; The RFC says the patch operations are applied to the object in sequence, but also that if
;; any operations "fail" - i.e. the test fails or an entry doesn't exist - then the
;; whole patch application is essentially rolled back and the input is unchanged.
;; I think to do it properly means making a deep-copy, applying the patch to it, and then, only if it succeeds,
;; apply the operations to the input.
;; In the meantime, this makes a copy and returns the copy unless the operations fail, then it returns a copy of the original.
(defun apply-patch (obj patch)
  "Apply an RFC6902 JSON patch to obj.
https://www.rfc-editor.org/info/rfc6902/"
  (loop
    :with new-obj = (deep-copy obj)
    :with current-value = new-obj
    :with valid = t

    :for cmd :in (ensure-list patch)
    :for op = (at cmd :op)
    :for op-function = (assoc-value *ops* op :test #'string=)
    :do (setf (values valid current-value) (funcall op-function current-value cmd))
    :while valid
    :finally (return (values (if valid new-obj (deep-copy obj)) valid))))
