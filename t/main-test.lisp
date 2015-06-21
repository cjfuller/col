(in-package :cl-user)

(defpackage :col-test
  (:use :cl :col :fiveam))

(in-package :col-test)

(enable-map-literals)

(def-suite :col)
(in-suite :col)

;; TODO: split out tests by package

(defclass test-obj-with-slots ()
  ((test0 :initarg :test0)
   (test1 :initarg :test1)
   (test2 :initarg :test2)))

(test mget-test
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

    (is (equal (mget pl-test :test1) "v1"))
    (is (equal (mget al-test :test1) "v1"))
    (is (equal (mget h-test :test1) "v1"))
    (is (equal (mget obj-test 'test1) "v1"))))

(test mget*-test
  (let ((test-obj '(:test0 ((:i0test0 . "v")))))
    (is (equal (mget* test-obj :test0 :i0test0) "v"))))

(test threading-test
  (flet ((helper-a (str) (concatenate 'string str "a"))
         (helper-b (str str1) (concatenate 'string str "b" str1))
         (helper-c (str) (concatenate 'string str "c")))
    (is (equal (-> "" helper-a (helper-b "x") helper-c) "abxc"))))

(test keys-test
    (let ((pl-test '(:test0 "v0" :test1 "v1" :test2 "v2"))
          (al-test '((:test0 . "v0") (:test1 . "v1") (:test2 . "v2")))
          (h-test (make-hash-table))
          (expected '(:test0 :test1 :test2)))
      (setf (gethash :test0 h-test) "v0")
      (setf (gethash :test1 h-test) "v1")
      (setf (gethash :test2 h-test) "v2")

      (is (equal (keys pl-test) expected))
      (is (equal (keys al-test) expected))
      (is (equal (keys h-test) expected))))

(test ht-test
  (let ((h (ht :a 1 :b 2 :c 3)))
    (is (equal (type-of h) 'hash-table))
    (is (equal (keys h) '(:a :b :c)))
    (is (equal (mget h :b) 2))))

(test plist->alist-test
  (let ((lst '(1 2 3 4)))
    (is (equal (plist->alist lst) '((1 . 2) (3 . 4))))))

(test hset-test
  (let ((h (make-hash-table)))
    (hset h :test 'val)
    (is (equal (mget h :test) 'val))))

(test remove-from-plist-test
  (let ((pl '(:a 1 :b 2 :c 3)))
    (is (equal (remove-from-plist pl :a) '(:b 2 :c 3)))
    (is (equal (remove-from-plist pl :b) '(:a 1 :c 3)))
    (is (equal (remove-from-plist pl :c) '(:a 1 :b 2)))))

(test map-literal-test
  (is (equal (mget #M{:hello "world"} :hello) "world"))
  (is (equal (mget #M{:hello :world} :hello) :world))
  (is (equal (mget* #M{:hello #M{:world "earth"}} :hello :world) "earth"))
  (is (equal (mget #M(:test #'eql){:hello "world"} :hello) "world"))
  (is (equal (mget #M(:test #'eql){(list "hello") "world"} '("hello")) nil))
  (is (equal (mget #M{(list "hello") "world"} '("hello")) "world")))

(test mset-test
  (:= test-fn (lambda (m) (is (equal (mget (mset m :hello "mars") :hello) "mars")))
    (funcall test-fn #M{:hello "world" :goodnight "moon"})
    (funcall test-fn '(:hello "world" :goodnight "moon"))
    (funcall test-fn '((:hello . "world") (:goodnight ."moon")))))

(test nthval-test
  (is (equal (nthval (values "a" "b" "c") 1) "b")))

(pop-reader-exts)
