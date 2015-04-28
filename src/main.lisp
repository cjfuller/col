(in-package :cl-user)

(defpackage :cjf-stdlib
  (:use :cl)
  (:export :mget :mget* :println :-> :->string :keys :ht)
  )

(defpackage :cjf-stdlib-test
  (:use :cl :cjf-stdlib)
  (:export :mget-test :mget*-test :threading-test))

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

;; TODO: probably should use generic functions
(defun mget (mapping key)
  "Get the value assocated with the given key from the given mapping.
Handles alists, plists, standard objects, and hashes (but doesn't check for well-formedness)."
  (cond
    ((alist-p mapping) (cdr (assoc key mapping)))
    ((plist-p mapping) (getf mapping key))
    ((hash-table-p mapping) (gethash key mapping))
    (t (slot-value mapping key))
    ))

(defun mget* (mapping &rest keys)
  "Like mget, but takes a series of keys for dealing with nested mappings.
The first key corresponds to the outermost layer of mapping."
  (if (cdr keys)
      (apply #'mget* (mget mapping (car keys)) (cdr keys))
      (mget mapping (car keys))))

(defgeneric ->string (obj)
  (:documentation "Convert an object to a string."))

(defmethod ->string (obj)
  obj)

(defun println (s)
  "Princ, followed by a newline.

Calls ->string on the object passed in.  (Default implementation is the
identity function.)
"
  (princ (->string s))
  (terpri))

(defmacro -> (first &body rest)
  "Clojure-style threading macro.  Evaluate the first form insert into the
position of the first argument of the second form and repeat while forms
remain.

If one of the arguments is a function name and not a list, the function is
called with the inserted value as the only argument.
"
  (if rest
      (if (listp (car rest))
          ;; Insert first into the first argument position
          (let ((fn-name (caar rest))
                (args (cdar rest)))
            `(-> (,fn-name ,first ,@args) ,@(cdr rest)))
          ;; Just call the function on first
          `(-> (,(car rest) ,first) ,@(cdr rest)))
      first))

(defgeneric keys (obj)
  (:documentation "Get the keys from a mapping (alist, plist, or hash table)."))

(defmethod keys ((h hash-table))
  (loop for key being the hash-keys of h collect key))

(defmethod keys ((l list))
  (if (alist-p l)
      (mapcar #'car l)
      (loop for i in l by #'cddr collect i)))

(defun ht (&rest plist)
  (let ((h (make-hash-table)))
    (mapc (lambda (k) (setf (gethash k h) (mget plist k))) (keys plist))
    h))

(in-package :cjf-stdlib-test)

(defclass test-obj-with-slots ()
  ((test0 :initarg :test0)
   (test1 :initarg :test1)
   (test2 :initarg :test2)))

(defun mget-test ()
  (let ((pl-test '(:test0 "v0" :test1 "v1" :test2 "v2"))
        (al-test '((:test0 . "v0") (:test1 . "v1") (:test2 . "v2")))
        (h-test (make-hash-table))
        (obj-test (make-instance 'test-obj-with-slots
                                 :test0 "v0"
                                 :test1 "v1"
                                 :test2 "v2")))
    (setf (gethash :test0 h-test) "v0")
    (setf (gethash :test1 h-test) "v1")
    (setf (gethash :test2 h-test) "v2")

    (assert (equal (mget pl-test :test1) "v1"))
    (assert (equal (mget al-test :test1) "v1"))
    (assert (equal (mget h-test :test1) "v1"))
    (assert (equal (mget obj-test 'test1) "v1"))))

(defun mget*-test ()
  (let ((test-obj '(:test0 ((:i0test0 . "v")))))
    (assert (equal (mget* test-obj :test0 :i0test0) "v"))))

(defun helper-a (str)
  (concatenate 'string str "a"))
(defun helper-b (str str1)
  (concatenate 'string str "b" str1))
(defun helper-c (str)
  (concatenate 'string str "c"))

(defun threading-test ()
  (assert (equal (-> "" helper-a (helper-b "x") helper-c) "abxc")))

(defun keys-test ()
    (let ((pl-test '(:test0 "v0" :test1 "v1" :test2 "v2"))
          (al-test '((:test0 . "v0") (:test1 . "v1") (:test2 . "v2")))
          (h-test (make-hash-table))
          (expected '(:test0 :test1 :test2)))
      (setf (gethash :test0 h-test) "v0")
      (setf (gethash :test1 h-test) "v1")
      (setf (gethash :test2 h-test) "v2")

      (assert (equal (keys pl-test) expected))
      (assert (equal (keys al-test) expected))
      (assert (equal (keys h-test) expected))))

(defun ht-test ()
  (let ((h (ht :a 1 :b 2 :c 3)))
    (assert (equal (type-of h) 'hash-table))
    (assert (equal (keys h) '(:a :b :c)))
    (assert (equal (mget h :b) 2))))
