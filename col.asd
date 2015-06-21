(asdf:defsystem col
  :description "A modular library of collected utilities, including a common interface for mapping types, clojure-style threading macro, and map literals."
  :version "0.1.0"
  :license "MIT"
  :depends-on (:cl-interpol :inferior-shell :alexandria :fset)
  :components
  ((:module "src"
    :components ((:file "main"
                  :depends-on ("mappings" "strings" "threading" "io" "reader" "misc"))
                 (:file "mappings")
                 (:file "strings")
                 (:file "threading")
                 (:file "io"
                  :depends-on ("strings"))
                 (:file "reader")
                 (:file "misc")))))
