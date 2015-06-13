(in-package :cl-user)

(defpackage :cjf-stdlib-test
  (:use :cl :cjf-stdlib)
  (:export :mget-test :mget*-test :threading-test :plist->alist-test :hset-test))

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

(defun remove-from-plist-test ()
  (let ((pl '(:a 1 :b 2 :c 3)))
    (assert (equal (remove-from-plist pl :a) '(:b 2 :c 3)))
    (assert (equal (remove-from-plist pl :b) '(:a 1 :c 3)))
    (assert (equal (remove-from-plist pl :c) '(:a 1 :b 2)))))

(defun map-literal-test ()
  (assert (equal (mget #M{:hello "world"} :hello) "world"))
  (assert (equal (mget #M{:hello :world} :hello) :world))
  (assert (equal (mget* #M{:hello #M{:world "earth"}} :hello :world) "earth"))
  (assert (equal (mget #M(:test #'eql){:hello "world"} :hello) "world"))
  (assert (equal (mget #M(:test #'eql){(list "hello") "world"} '("hello")) nil))
  (assert (equal (mget #M{(list "hello") "world"} '("hello")) "world")))
