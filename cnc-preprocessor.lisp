(defclass binding ()
  ((name :initarg :name
         :reader binding-name)
   (value :initarg :value
          :reader binding-value)
   (line-number :initarg :line-number
                :reader binding-line-number))
  (:default-initargs
   :name (error "Must supply name")
    :value (error "Must supply value")
    :line-number (error "Must supply line number"))
  (:documentation
   "A single variable binding"))

(defparameter *bindings* nil
  "An alist of all of the variable bindings, of form (NAME . BINDING)")

(defun make-binding (name value line-number)
  "Returns an instance of a variable binding"
  (declare (type simple-string name)
           (type fixnum line-number)
           (type (or single-float double-float) value))
  (make-instance 'binding
                 :name name
                 :value value
                 :line-number line-number))

(defun add-binding (binding)
  "Adds BINDING to *BINDINGS*"
  (setf *bindings* (acons (binding-name binding) binding
                          *bindings*)))

(defun find-binding (name)
  "Returns a binding with name NAME if it exists, returns NIL
otherwise"
  ;; This works because the cdr of NIL is also NIL
  (cdr (assoc name *bindings* :test #'string-equal)))

(defun string-trim-whitespace (string)
  "Trims whitespace from the left and right of STRING"
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun extract-name-from-line (line)
  "Returns the name part of a variable declaration line"
  (string-trim-whitespace
   (subseq line
           (1+ (position #\( line))
           (position #\= line))))

(defun extract-value-from-line (line)
  "Returns the value part of a variable declaration line"
  (float (read-from-string
          (string-trim-whitespace
           (subseq line
                   (1+ (position #\= line))
                   (position #\) line))))))

(define-condition malformed-declaration (condition)
  ((line :initarg :line :reader line)))

(defun declarationp (line)
  "Returns T if LINE contains a variable declaration"
  (and (search "(" line)
       (search "=" line)
       (search ")" line)))

(defun well-formed-declaration-p (line)
  "Return T if the line is a well-formed variable declaration and NIL otherwise"
  (let ((start-position (position #\( line))
        (equals-position (position #\= line))
        (end-position (position #\) line)))
    ;; This is just all of the checks I could think of
    (and (> (- equals-position start-position) 1)
         (every #'alpha-char-p
                (string-trim-whitespace
                 (subseq line (1+ start-position) equals-position)))
         (> (- end-position equals-position) 1)
         (every #'digit-char-p
                (string-trim-whitespace
                 (subseq line (1+ equals-position) end-position))))))

(defun parse-declaration (line)
  "Returns the name and value of a declaration line of form
(NAME = VALUE)"
  (if (well-formed-declaration-p line)
      (values
       (extract-name-from-line line)
       (extract-value-from-line line))
      (error 'malformed-declaration :line line)))

(defun read-file-for-bindings (stream)
  "Reads the contents of STREAM and builds a list of bindings defined in
  it. After the function returns, STREAM will be reset to position 0"
  (file-position stream 0)
  (loop for line = (read-line stream nil nil)
     for i = 1 then (1+ i)
     never (null line)
     do (when (declarationp line)
          (handler-case
              (multiple-value-bind (name value) (parse-declaration line)
                (let ((existing-binding (find-binding name)))
                  (if existing-binding
                      (progn
                        (format t "~&Variable ~A already defined on line ~A"
                                name (binding-line-number existing-binding))
                        (format t "~&Existing value: ~A"
                                (binding-value existing-binding)))
                      (add-binding (make-binding name value i)))))
            (malformed-declaration ()
              (format t "~&Malformed declaration ~A on line ~A. Aborting"
                      line i)))))
  (file-position stream 0))

(defun variable-expansion-p (line)
  "Returns T if LINE is a candidate for variable expansion, which is to say it
is of the form {VARIABLE-NAME}"
  (let ((start-position (position #\{ line))
        (end-position (position #\} line)))
    (and start-position
         end-position
         (> (- end-position start-position) 1)
         (every #'alpha-char-p
                (subseq line (1+ start-position) end-position)))))

(define-condition nonexistent-binding (condition)
  ((name :initarg :name :reader name)))

(defun expand-variables (line)
  "Returns a copy of LINE such that every variable has been expanded"
  (cond ((not (variable-expansion-p line)) line)
        (t (let* ((start-position (position #\{ line))
                  (end-position (position #\} line))
                  (variable-name (subseq line
                                         (1+ start-position)
                                         end-position))
                  (binding (find-binding variable-name)))
             (if (null binding)
                 (error 'nonexistent-binding :name variable-name)
                 (concatenate 'string
                              (subseq line 0 start-position)
                              (princ-to-string (binding-value binding))
                              (expand-variables
                               (subseq line (1+ end-position)))))))))

(defun read-file-for-expansions (in-stream out-stream)
  "Passes over IN-STREAM and sends its contents to OUT-STREAM with all variables
expanded"
  (file-position in-stream 0)
  (loop for line = (read-line in-stream nil nil)
     never (null line)
     do (handler-case
            (write-line (expand-variables line) out-stream)
          (nonexistent-binding (condition)
            (format t "~&Variable ~A never defined" (name condition))))))

(defun prompt (prompt)
  (declare (type simple-string prompt))
  (format t "~A> " prompt)
  (read-line))

(defun main ()
  (let* ((in-filename (prompt "Enter input filename"))
         (out-filename (concatenate 'string
                                    in-filename
                                    ".preprocessed")))
    (when (not (probe-file in-filename))
      (format t "File ~A doesn't exist" in-filename)
      (quit))
    (with-open-file (in in-filename)
      (read-file-for-bindings in)
      (with-open-file (out out-filename
                           :direction :output
                           :if-exists :supersede)
        (read-file-for-expansions in out)))
    (format t "Wrote output on ~A" out-filename)))
