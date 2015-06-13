(asdf:defsystem cjf-stdlib
  :description "Personal standard library."
  :version "0.0.1"
  :license "MIT"
  :depends-on (:cl-interpol :inferior-shell :split-sequence :alexandria)
  :components
  ((:module "src"
    :components
    ((:file "main")))))
