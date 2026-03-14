(in-package :functional-json)

;; Readers

;; Currently supports reading from a stream, string, or file (pathname).

(defgeneric read-json (in &optional junk-allowed-p)
  (:documentation "Read a JSON-encoded value from an input source."))

(defun is-whitespace (char)
  "Check if char is whitespace."
  #+sbcl (sb-unicode:whitespace-p char)
  #-sbcl(member char '(#\space #\newline #\return #\tab)))

(defun ends-atom (char)
  "Returns non-nil if char ends a JSON atom, nil otherwise."
  (or (is-whitespace char) (member char '(#\) #\] #\} #\, #\:))))


(defun skip-whitespace (stream)
  "Skip whitespace (and C++ style comments if *allow-comments*) and read to the first
JSON data."
  (declare #.*optimize*)
  (loop :for char := (peek-char nil stream nil)
        :while (cond
                 ((and char (is-whitespace char)) (read-char stream))

                 ;; Skip comments
                 ((and *allow-comments* char (char= #\/ char))
                  (read-char stream)
                  (unless (char= #\/ (peek-char nil stream nil))
                    (raise 'json-parse-error "Unexpected input '/~A'." (read-char stream)))
                  ;; Skip the second slash.
                  (read-char stream)
                  (loop :while (not (member (peek-char nil stream nil)
                                            '(#\newline #\return)))
                        :do (read-char stream))
                  (read-char stream)
                  ))))

(defun at-eof (stream)
  "Check if stream is at its end.  If non-nil, there is data to read."
  (eql (peek-char nil stream nil :eof) :eof))

(declaim (inline read-json-element read-json-string))
(defun read-json-string (stream)
  "Read a JSON UTF-8 string value from stream."
  (declare #.*optimize*)
  (labels ((interpret (char)
             (if (eql char #\\)
                 (let ((escaped (read-char stream)))
                   (case escaped
                     (#\u (read-unicode))
                     (#\b #\backspace) (#\n #\newline) (#\r #\return)
                     (#\t #\tab) (#\f #\page) (t escaped)))
                 char))
           (read-unicode ()
             ;; refer to ECMA-404, strings.
             (flet ((read-code-point ()
                      (the fixnum
                           (loop :for pos :of-type fixnum :from 0 :below 4
                                 :for weight :of-type fixnum := #.(expt 16 3) :then (ash weight -4)
                                 :for digit  := (digit-char-p (read-char stream) 16)
                                 :do (unless digit (raise 'json-parse-error "Invalid unicode constant in string."))
                                 :sum (* digit weight) :into total :of-type fixnum
                                 :finally (return total))))
                    (expect-char (char)
                      (let ((c (read-char stream)))
                        (assert (char= char c) (c)
                                "Expecting ~c, found ~c instead." char c))))
               (let ((code-point (read-code-point)))
                 (code-char
                  (if (<= #xD800 code-point #xDBFF)
                      (let ((utf-16-high-surrogate-pair code-point))
                        (expect-char #\\)
                        (expect-char #\u)
                        (let ((utf-16-low-surrogate-pair (read-code-point)))
                          (declare (type fixnum utf-16-low-surrogate-pair))
                          (assert (<= #xDC00 utf-16-low-surrogate-pair #xDFFF)
                                  (utf-16-low-surrogate-pair)
                                  "Unexpected UTF-16 surrogate pair: ~a and ~a."
                                  utf-16-high-surrogate-pair
                                  utf-16-low-surrogate-pair)
                          (+ #x010000
                             (ash (- utf-16-high-surrogate-pair #xD800) 10)
                             (- utf-16-low-surrogate-pair #xDC00))))
                      code-point))))))
    (with-output-to-string (out)
      (handler-case
          (loop :with quote :of-type character := (read-char stream)
                :for next :of-type character := (read-char stream)
                :until (eql next quote)
                :do (write-char (interpret next) out))
        (end-of-file () (raise 'json-eof-error "Encountered end of input inside string constant."))))))

(defun read-json-element (stream)
  "Read the next JSON entity from stream."
  (declare #.*optimize*)
  (skip-whitespace stream)
  (case (peek-char nil stream nil :eof)
    (:eof (raise 'json-eof-error "Unexpected end of input."))
    ((#\" #\') (read-json-string stream))
    (#\[ (read-json-list stream))
    (#\{ (read-json-object stream))
    (t (read-json-atom stream))))



(declaim (inline gather-comma-separated))
(defun gather-comma-separated (stream end-char obj-name gather-func)
  "Call #'gather-func on each comma separated value until reaching end-char.
obj-name is used for printing errors, and typically specifies the type of object."
  ;; TODO: Should this be a macro?
  ;; It feels like this is critical functionality, and I'm not sure it optimizes well
  (declare #.*optimize*)
  (declare (type character end-char)
           (type (function () t) gather-func))
  ;; Throw away opening char
  (read-char stream)
  (let ((finished nil))
    (loop
      (skip-whitespace stream)
      (let ((next (peek-char nil stream nil #\nul)))
        (declare (type character next))
        (when (eql next #\nul)
          (raise 'json-eof-error "Encountered end of input inside ~A." obj-name))
        (when (eql next end-char)
          (read-char stream)
          (return))
        (when finished
          (raise 'json-parse-error "Comma or end of ~A expected, found '~A'" obj-name next)))
      (funcall gather-func)
      (skip-whitespace stream)
      (if (eql (peek-char nil stream nil) #\,)
          (read-char stream)
          (setf finished t)))))

(defun read-json-list (stream)
  "Read a JSON list from stream.
Creates a list if *decode-lists-as* is :list (the default)
and an array if *decode-lists-as* is :array."
  (declare #.*optimize*)
  (let ((accum ()))
    (gather-comma-separated stream #\] "list"
                            (lambda ()
                              (push (read-json-element stream) accum)))
    (ecase *decode-lists-as*
      (:list (nreverse accum))
      (:array (make-array (length accum) :initial-contents (nreverse accum))))))

(defun read-json-object (stream)
  "Read a JSON object from stream.
Creates an association list if *decode-objects-as* is :jso or :alist.
Creates a hashtable if *decode-objects-as* is :hashtable.
Note that many functions in functional-json don't work on :hashtable objects yet. "
  (declare #.*optimize*)
  (ecase *decode-objects-as*
    ((:jso :alist)
     (let ((accum ()))
       (gather-comma-separated
        stream #\} "object literal"
        (lambda ()
          (let ((slot-name (let ((*reading-slot-name* t)) (read-json-element stream))))
            (unless (or (stringp slot-name) (numberp slot-name))
              (raise 'json-parse-error "Invalid slot name in object literal: ~A" slot-name))
            (skip-whitespace stream)
            (when (not (eql (read-char stream nil) #\:))
              (raise 'json-parse-error "Colon expected after '~a'." slot-name))
            (push (cons slot-name (read-json-element stream)) accum))))
       (if (eq *decode-objects-as* :jso)
           (make-jso :alist (nreverse accum))
           (nreverse accum))))
     (:hashtable
      (let ((accum (make-hash-table :test #'equal)))
        (gather-comma-separated
         stream #\} "object literal"
         (lambda ()
           (let ((slot-name (let ((*reading-slot-name* t)) (read-json-element stream))))
             (unless (or (stringp slot-name) (numberp slot-name))
               (raise 'json-parse-error "Invalid slot name in object literal: ~A" slot-name))
             (skip-whitespace stream)
             (when (not (eql (read-char stream nil) #\:))
               (raise 'json-parse-error "Colon expected after '~a'." slot-name))
             (setf (gethash slot-name accum) (read-json-element stream)))))
        accum))))

(defun looks-like-a-number (string)
  "Check if the content of string looks like an integer or floating point number."
  (declare #.*optimize*)
  (declare (type (or sequence (array character (*)) simple-string string)))
  (every (lambda (char)
           (or (digit-char-p char)
               (member char '(#\e #\E #\. #\- #\+))))
         string))

(defun read-json-atom (stream)
  "Read a number, true, false, null, or undefined from stream"
  (declare #.*optimize*
           (type stream stream))
  (let ((accum (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
     (let ((next (peek-char nil stream nil :eof)))
       (when (or (ends-atom next) (eql next :eof))
         (return))
       (vector-push-extend next accum)
       (read-char stream)))
    (let ((number-val (and (looks-like-a-number accum)
                           (ignore-errors (read-from-string accum)))))
      (cond ((numberp number-val) number-val)
            ((string= accum "false") :false)
            ((string= accum "true") :true)
            ((string= accum "null") :null)
            ((string= accum "undefined") :null)
            ((and *reading-slot-name*
                  (every (lambda (c)
                           (declare (type character c))
                           (or (alphanumericp c) (eql c #\_) (eql c #\$)))
                         accum))
             accum)
            (t (raise 'json-parse-error "Unrecognized value in JSON data: ~A" accum))))))

(defmethod read-json ((in stream) &optional (junk-allowed-p t))
  "Read a JSON value from stream."
  (declare #.*optimize*)
  (let ((value (read-json-element in)))
    (skip-whitespace in)
    (unless (or junk-allowed-p (at-eof in))
      (raise 'json-parse-error "Unused characters at end of input."))
    value))

(defmethod read-json ((in string) &optional (junk-allowed-p nil))
  "Read a JSON value from a string."
  (declare #.*optimize*)
  (with-input-from-string (stream in)
    (read-json stream junk-allowed-p)))

(defmethod read-json ((file-name pathname) &optional (junk-allowed-p nil))
  "Read JSON from a file."
  (declare #.*optimize*)
  (with-open-file (stream file-name :direction :input)
    (read-json stream junk-allowed-p)))
