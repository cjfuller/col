(asdf:defsystem cjf-stdlib
  :description "Personal standard library."
  :version "0.0.1"
  :license "MIT"
  :depends-on (:cl-interpol :inferior-shell)
  :components
  ((:module "src"
    :components
    ((:file "main")))))
