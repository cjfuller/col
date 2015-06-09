(in-package :cl-user)

(defpackage :cjf-stdlib
  (:use :cl :split-sequence)
  (:export :mget :mget* :println :-> :->string :keys :ht :plist->alist :hset
   :slurp :alist->ht :<- :mset!
   ; re-export from split-sequence
   :split-sequence :split-sequence-if :split-sequence-if-not
   ))

(defpackage :cjf-stdlib-test
  (:use :cl :cjf-stdlib)
  (:export :mget-test :mget*-test :threading-test :plist->alist-test :hset-test))

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
    ((alist-p mapping) (cdr (assoc key mapping :test #'equal)))
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

(defun hset (hash key value)
  "A slightly more convenient shorthand for setting values in a hash table."
  (setf (gethash key hash) value))

(defgeneric mset! (obj key value)
  (:documentation "Set the value associated with a key in a mapping in-place."))

(defmethod mset! ((obj hash-table) key value)
  (hset h key value))

(defmethod mset! ((obj list) key value)
  ;; Fail if it's not an alist
  (unless (listp (car obj))
    (error "Can only use mset! on alists."))
  (if (assoc key obj)
      (setf (cdr (assoc key obj)) value)
      (nconc obj (list (cons key value)))))

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

(defmacro rmapcar (lst fn)
  "Mapcar with arguments reversed.  Allows use with threading macro."
  `(mapcar ,fn ,lst))

(defgeneric keys (obj)
  (:documentation "Get the keys from a mapping (alist, plist, or hash table)."))

(defmethod keys ((h hash-table))
  (loop for key being the hash-keys of h collect key))

(defmethod keys ((l list))
  (if (alist-p l)
      (mapcar #'car l)
      (loop for i in l by #'cddr collect i)))

;; TODO: equality test, etc.
(defun ht (&rest plist)
  (let ((h (make-hash-table)))
    (mapc (lambda (k) (setf (gethash k h) (mget plist k))) (keys plist))
    h))

(defun plist->alist (plist)
  (loop for x in plist by #'cddr
        for y in (cdr plist) by #'cddr
        collect (cons x y)))

(defun alist->ht (alist &key (test #'eql))
  (let ((h (make-hash-table :test test)))
    (loop for pair in alist
          do (hset h (car pair) (cdr pair)))
    h))

(defun shell-command-reader (stream char arg)
  (let ((str (cl-interpol::interpol-reader stream char arg)))
    `(inferior-shell:run/ss ,str)))

;; TODO: make this enableable
(set-dispatch-macro-character #\# #\! #'shell-command-reader)

(defun slurp (fn &key (lines nil))
  "Slurp a file into a strng.

If lines is non-nil, return a list, one string per line (newlines stripped)."
  (with-open-file (f fn)
    (if lines
        (uiop:slurp-stream-lines f)
        (uiop:slurp-stream-string f))))

(defmacro <- (var val &body body)
  `(let ((,var ,val))
     ,@body))

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

(defun plist->alist-test ()
  (let ((lst '(1 2 3 4)))
    (assert (equal (plist->alist lst) '((1 . 2) (3 . 4))))))

(defun hset-test ()
  (let ((h (make-hash-table)))
    (hset h :test 'val)
    (assert (equal (mget h :test) 'val))))
