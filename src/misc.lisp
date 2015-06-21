(in-package :cl-user)

(defpackage :col.misc
  (:use :cl)
  (:export := :nthval :nthvals))

(in-package :col.misc)

(defmacro := (var val &body body)
  "Shorthand for let for a single binding."
  `(let ((,var ,val))
     ,@body))

(defmacro nthval (values-returning n)
  "Get the value at the given position."
  `(car (nthvals ,values-returning (,n))))

(defmacro nthvals (values-returning nlist)
  "Get the values at the given positions as a list."
  (let* ((max-n (1+ (apply #'max nlist)))
         (symbols (alexandria:make-gensym-list max-n)))
    `(multiple-value-bind ,symbols
         ;'(declare (ignorable *))  TODO: how do I do this?
         ,values-returning
       (list ,@(mapcar (lambda (n) (nth n symbols)) nlist)))))

