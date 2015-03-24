(in-package :cl-user)

(defpackage :cjf-stdlib
  (:use :cl)
  (:export :mget :mget* :println)
  )

(defpackage :cjf-stdlib-test
  (:use :cl :cjf-stdlib)
  (:export :mget-test :mget*-test))

(in-package :cjf-stdlib)

(defun alist-p (mapping)
  "Shortcut for determining if a mapping is an alist, given that it must be one
of an alist, plist, or hash."
  (and (listp mapping)
       (listp (car mapping))))

(defun plist-p (mapping)
  "Shortcut for determining if a mapping is a plist, given that it must be one
of an alist, plist, or hash."
  (and (listp mapping)
       (symbolp (car mapping))))

(defun mget (mapping key)
  "Get the value assocated with the given key from the given mapping.
Handles alists, plists, and hashes, (but doesn't check for well-formedness)."
  (cond
    ((alist-p mapping) (cdr (assoc key mapping)))
    ((plist-p mapping) (getf mapping key))
    (t (gethash key mapping))))

(defun mget* (mapping &rest keys)
  "Like mget, but takes a series of keys for dealing with nested mappings.
The first key corresponds to the outermost layer of mapping."
  (if (cdr keys)
      (apply #'Mget* (mget mapping (car keys)) (cdr keys))
      (mget mapping (car keys))))

(defun println (s)
  "Princ, followed by a newline."
  (princ s)
  (terpri))

(in-package :cjf-stdlib-test)

(defun mget-test ()
  (let ((pl-test '(:test0 "v0" :test1 "v1" :test2 "v2"))
        (al-test '((:test0 . "v0") (:test1 . "v1") (:test2 . "v2")))
        (h-test (make-hash-table)))
    (setf (gethash :test0 h-test) "v0")
    (setf (gethash :test1 h-test) "v1")
    (setf (gethash :test2 h-test) "v2")

    (assert (equal (mget pl-test :test1) "v1"))
    (assert (equal (mget al-test :test1) "v1"))
    (assert (equal (mget h-test :test1) "v1"))))

(defun mget*-test ()
  (let ((test-obj '(:test0 ((:i0test0 . "v")))))
    (assert (equal (mget* test-obj :test0 :i0test0) "v"))))
