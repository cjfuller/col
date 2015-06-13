(asdf:defsystem cjf-stdlib-test
  :description "Personal standard library tests."
  :version "0.0.1"
  :license "MIT"
  :depends-on (:cjf-stdlib)
  :components
  ((:module "t"
    :components
    ((:file "main-test")))))
