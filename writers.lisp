
(in-package :functional-json)

;; Writer and pretty printer

;; TODO: Combine pretty and non-pretty printed implementation

(defparameter *print-object-style* :pretty
  "Set this to control the print style of print-object.
  :pretty - pretty prints using print-json-element
  :one-line - writes to a single line using write-json-element
  any other value uses the built-in structure style.")


(defparameter *pretty-print-indent-size* 4
  "The number of spaces to use when indenting pretty printed JSON.")

(defparameter *script-tag-hack* nil
  "Bind this to T when writing JSON that will be written to an HTML
  document. It prevents '</script>' from occurring in strings by
  escaping any slash following a '<' character.")

(defparameter *output-literal-unicode* nil
  "Bind this to T in order to reduce the use of \uXXXX Unicode escapes,
  by emitting literal characters (encoded in UTF-8). This may help
  reduce the parsing effort for any recipients of the JSON output, if
  they can already read UTF-8, or else, they'll need to implement
  complex unicode (eg UTF-16 surrogate pairs) escape parsers.")

(defmethod print-object ((object jso) stream)
  (case *print-object-style*
    (:pretty
     (print-json-element object stream))
    (:one-line
     (write-json object stream))
    (t
     (call-next-method))))

(defgeneric print-json-element (element stream &optional indent)
  (:method (element stream &optional (indent 0))
    (declare (ignore stream indent))
    (error 'json-write-error "Can not pretty-print object of type ~A as JSON." (type-of element)))
  (:documentation "Method used for pretty printing values of a specific type.
  You can specialise this for your own types."))

(defgeneric write-json-element (element stream)
  (:method (element stream)
    (declare (ignore stream))
    (raise 'json-write-error "Can not write object of type ~A as JSON." (type-of element)))
  (:documentation "Method used for writing values of a specific type.
  You can specialise this for your own types."))


(defun write-json-to-string (element)
  "Write a value's JSON representation to a string."
  (with-output-to-string (out)
    (write-json element out)))

(defun write-json (element stream)
  "Write a value's JSON representation to a stream."
  (let ((*print-pretty* nil))
    (write-json-element element stream)
    (values)))

(defmethod write-json-element ((element symbol) stream)
  ;; (declare #.*optimize*)
  (ecase element
    ((nil) (write-string "[]" stream))
    ((t :true) (write-string "true" stream))
    (:false (write-string "false" stream))
    ((:null :undefined) (write-string "null" stream))))

(defmethod write-json-element ((element string) stream)
  ;; (declare #.*optimize*
  ;;          (type stream stream))
  (let ((element (coerce element 'simple-string)))
    (write-char #\" stream)
    (loop :for prev := nil :then ch
       :for ch :of-type character :across element :do
       (let ((code (char-code ch)))
         (declare (fixnum code))
         (if (or (<= 0 code #x1f)
                 (<= #x7f code #x9f))
             (case code
               (#.(char-code #\backspace) (write-string "\\b" stream))
               (#.(char-code #\newline)   (write-string "\\n" stream))
               (#.(char-code #\return)    (write-string "\\r" stream))
               (#.(char-code #\page)      (write-string "\\f" stream))
               (#.(char-code #\tab)       (write-string "\\t" stream))
               (t                         (format stream "\\u~4,'0x" code)))
             (case code
               (#.(char-code #\/)  (when (and (eql prev #\<) *script-tag-hack*)
                                     (write-char #\\ stream))
                                   (write-char ch stream))
               (#.(char-code #\\)  (write-string "\\\\" stream))
               (#.(char-code #\")  (write-string "\\\"" stream))
               (t                  (cond ((< #x1F code #x7F)
                                          (write-char ch stream))
                                         ((and (< #x9F code #x10000)
                                               (not *output-literal-unicode*))
                                          (format stream "\\u~4,'0x" code))
                                         ((and (< #x10000 code #x1FFFF)
                                               (not *output-literal-unicode*))
                                          (let ((c (- code #x10000)))
                                            (format stream "\\u~4,'0x\\u~4,'0x"
                                                    (logior #xD800 (ash c -10))
                                                    (logior #xDC00 (logand c #x3FF)))))
                                         (t
                                          (write-char ch stream))))))))
    (write-char #\" stream)))

#+nil
(let ((functional-json:*script-tag-hack* t))
  (functional-json:write-json-to-string "Test 𝄞 ⇓ 	<tag>
</tag>"))
;; ==> "\"Test \\uD834\\uDD1E \\u21D3 \\t<tag>\\n<\\/tag>\""

(defmethod write-json-element ((element integer) stream)
  (write element :stream stream))

(defmethod write-json-element ((element real) stream)
  (format stream "~,,,0,,,'eE" element))

(defmethod write-json-element ((element hash-table) stream)
  ;; (declare #.*optimize*)
  (write-json-element
   (make-jso :alist (loop :for key :being :the :hash-keys :of element :using (hash-value val)
                          :collect (cons key val)))
   stream))

(defmethod write-json-element ((element jso) stream)
  ;; (declare #.*optimize*)
  (write-char #\{ stream)
  (loop :for (key . val) :in (jso-alist element)
        :for first := t :then nil
        :unless first :do (write-char #\, stream)
        :do (write-json-element key stream)
        :do (write-char #\: stream)
        :do (write-json-element val stream))
  (write-char #\} stream))

(defmethod write-json-element ((element list) stream)
  ;; (declare #.*optimize*)
  (write-char #\[ stream)
  (let ((first t))
    (dolist (part element)
     (if first
         (setf first nil)
         (write-char #\, stream))
     (write-json-element part stream)))
  (write-char #\] stream))



;; Pretty printing

(declaim (inline print-indent))
(defun print-indent (indentation stream)
    (write-string (make-string (* (max 0 indentation) *pretty-print-indent-size*) :initial-element #\Space) stream))

(defmethod print-json-element ((element symbol) stream &optional (indent 0))
  (declare (ignorable indent))

  (ecase element
    ((nil) (write-string "[]" stream))
    ((t :true) (write-string "true" stream))
    (:false (write-string "false" stream))
    ((:null :undefined) (write-string "null" stream))))

(defmethod print-json-element ((element string) stream &optional (indent 0))
  (declare (ignorable indent))
  (write-json-element element stream))

(defmethod print-json-element ((element integer) stream &optional (indent 0))
  (declare (ignorable indent))
  (write element :stream stream))

(defmethod print-json-element ((element real) stream &optional (indent 0))
  (declare (ignorable indent))
  (format stream "~,,,0,,,'eE" element))

(defmethod print-json-element ((element hash-table) stream &optional (indent 0))
  (declare (type fixnum indent))
  (print-indent indent stream)
  (print-json-element
   (make-jso :alist (loop
                      :for key :being :the :hash-keys :of element
                        :using (hash-value val)
                      :collect (cons key val)))
   stream
   (1+ indent)))

(defmethod print-json-element ((element jso) stream &optional (indent 0))
  (declare (type (unsigned-byte 32) indent)
           (type stream stream))
  (write-char #\{ stream)
  (write-char #\Newline stream)
  (print-indent (1+ indent) stream)
  (loop :for (key . val) :in (jso-alist element)
        :for first := t :then nil
        :unless first :do
          (write-char #\, stream)
          (write-char #\Newline stream)
          (print-indent (1+ indent) stream)
        :do (print-json-element key stream (1+ indent))
        :do (write-char #\: stream)
            (write-char #\Space stream)
        :do (print-json-element val stream (1+ indent)))
  (write-char #\Newline stream)
  (print-indent indent stream)
  (write-char #\} stream))

(defmethod print-json-element ((element list) stream &optional (indent 0))
  (declare (type fixnum indent))
  (write-char #\[ stream)
  (let ((first t))
    (dolist (part element)
      (cond
        (first
         (setf first nil))
        (t
         (write-char #\, stream)
         (write-char #\Newline stream)
         (print-indent indent stream)))
      (print-json-element part stream (1+ indent))))
  (print-indent (1- indent) stream)
  (write-string "]\\n" stream))

