(in-package :cl-user)

(defpackage :col.strings
  (:use :cl)
  (:export :->string :<>))

(in-package :col.strings)

(defun <> (&rest strings)
  "Shorthand for concatenating strings."
  (apply #'concatenate (cons 'string strings)))

(defgeneric ->string (obj)
  (:documentation "Convert an object to a string."))

(defmethod ->string (obj)
  "Default implementation: don't do anything."
  obj)
