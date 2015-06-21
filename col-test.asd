(asdf:defsystem col-test
  :description "Tests for the col system."
  :version "0.1.0"
  :license "MIT"
  :depends-on (:col :fiveam)
  :components
  ((:module "t"
    :components
    ((:file "main-test")))))
