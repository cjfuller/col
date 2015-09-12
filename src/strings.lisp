(in-package :cl-user)

(defpackage :col.strings
  (:use :cl)
  (:export :->string :<>))

(in-package :col.strings)

(cl-interpol:enable-interpol-syntax)

(defun <> (&rest strings)
  "Shorthand for concatenating strings."
  (apply #'concatenate (cons 'string strings)))

(defun ends-with? (str suffix)
  (and (cl-ppcre:scan #?r"${suffix}\Z" str) t))

(defun starts-with? (str prefix)
  (and (cl-ppcre:scan #?r"\A${prefix}" str) t))

(defgeneric ->string (obj)
  (:documentation "Convert an object to a string."))

(defmethod ->string (obj)
  "Default implementation: don't do anything."
  obj)

(cl-interpol:disable-interpol-syntax)
