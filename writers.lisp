(in-package :functional-json)

;; Writer and pretty printer

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

(defparameter *output-literal-unicode* t
  "Bind this to T in order to reduce the use of \uXXXX Unicode escapes,
  by emitting literal characters (encoded in UTF-8). This may help
  reduce the parsing effort for any recipients of the JSON output, if
  they can already read UTF-8, or else, they'll need to implement
  complex unicode (eg UTF-16 surrogate pairs) escape parsers.")

(defmethod print-object ((object jso) stream)
  (write-json object stream))

(defgeneric write-json-element (element stream depth)

  (:method (element stream depth)
    (declare (ignore stream))
    (raise 'json-write-error "Can not write object of type ~A as JSON." (type-of element)))

  (:method ((element integer) stream depth)
    (write element :stream stream))

  (:method ((element real) stream depth)
    (format stream "~,,,0,,,'eE" element))

  (:documentation "Method used for writing values of a specific type.
  You can specialise this for your own types."))


(defun write-json-to-string (element)
  "Write a value's JSON representation to a string."
  (with-output-to-string (out)
    (write-json element out)))

(defun write-json (element stream)
  "Write a value's JSON representation to a stream."
  (write-json-element element stream 0)
  (values))

(defmethod write-json-element ((element symbol) stream depth)
  ;; (declare #.*optimize*)
  (ecase element
    ((nil) (write-string "[]" stream))
    ((t :true) (write-string "true" stream))
    (:false (write-string "false" stream))
    ((:null :undefined) (write-string "null" stream))))

(defmethod write-json-element ((element string) stream depth)
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
  (functional-json:write-json-to-string "Test 𝄞 ⇓   <tag>
</tag>"))
;; ==> "\"Test \\uD834\\uDD1E \\u21D3 \\t<tag>\\n<\\/tag>\""

(defmethod write-json-element ((element hash-table) stream depth)
  ;; (declare #.*optimize*)
  (write-json-element
   (make-jso :alist (loop :for key :being :the :hash-keys :of element :using (hash-value val)
                          :collect (cons key val)))
   stream
   depth))

(defun indent-for-depth (depth)
  (* (max 0 depth)
     *pretty-print-indent-size*))

(defparameter *whitespace-cache* (make-hash-table :size 32)
  "Cache indentation whitespace strings so they don't need to be recreated.")

(defun indent-string (space-count)
  (when (null (gethash space-count *whitespace-cache*))
    (setf (gethash space-count *whitespace-cache*)
          (make-string space-count :initial-element #\space)))
  (gethash space-count *whitespace-cache*))

(defun write-whitespace (stream is-pretty depth)
  (when (eq :pretty is-pretty)
    (write-char #\Newline stream)
    (write-string (indent-string (indent-for-depth depth)) stream)))

(defmethod write-json-element ((element jso) stream depth)
  ;; (declare #.*optimize*)
  (let ((is-pretty *print-object-style*))
    (write-char #\{ stream)
    (write-whitespace stream is-pretty (1+ depth))
    (loop :for (key . val) :in (jso-alist element)
          :for first := t :then nil
          :unless first
            :do (write-char #\, stream)
                (write-whitespace stream is-pretty (1+ depth))
          :do
             (write-json-element key stream (1+ depth))
             (write-char #\: stream)
             (write-char #\space stream)
             (write-json-element val stream (1+ depth)))
    (write-whitespace stream is-pretty depth)
    (write-char #\} stream)))

(defmethod write-json-element ((element list) stream depth)
  ;; (declare #.*optimize*)

  (let ((is-pretty *print-object-style*))
    (write-char #\[ stream)
    (write-whitespace stream is-pretty (1+ depth))
    (loop
      :for object :in element
      :for first = t :then nil
      :when (not first) :do
        (write-char #\, stream)
        (write-whitespace stream is-pretty (1+ depth))
      :do
         (write-json-element object stream (1+ depth)))
    (write-whitespace stream is-pretty depth)
    (write-char #\] stream)))
