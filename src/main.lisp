(in-package :cl-user)

(defpackage :col
  (:use :cl
        :col.mapping :col.strings :col.threading :col.io :col.reader :col.misc))

(in-package :col)

(defun reexport-all (pkg)
  (do-external-symbols (sym (find-package pkg))
    (export sym)))

(reexport-all :col.mapping)
(reexport-all :col.strings)
(reexport-all :col.threading)
(reexport-all :col.io)
(reexport-all :col.reader)
(reexport-all :col.misc)
