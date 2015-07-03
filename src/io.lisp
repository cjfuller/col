(in-package :cl-user)

(defpackage :col.io
  (:use :cl :col.strings)
  (:export :println :slurp))

(in-package :col.io)

(defun println (s)
  "Princ, followed by a newline.

Calls ->string on the object passed in.  (Default implementation is the
identity function.)
"
  (princ (->string s))
  (terpri))

(defun slurp (fn &key (lines nil))
  "Slurp a file into a string.

If lines is non-nil, return a list, one string per line (newlines stripped)."
  (with-open-file (f fn)
    (if lines
        (uiop:slurp-stream-lines f)
        (uiop:slurp-stream-string f))))
