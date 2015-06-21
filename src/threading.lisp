(in-package :cl-user)

(defpackage :col.threading
  (:documentation "Clojure-style threading macro and associated utilities.")
  (:use :cl)
  (:export :-> :tee :tap :rmapcar))

(in-package :col.threading)

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

(defmacro tee (first &body rest)
  "Tee for the threading macro.  Insert first into the first position in each
of the body forms, then collect the results as a list."
  `(list ,@(mapcar (lambda (form)
                     `(-> ,first ,form))
                   rest)))

(defmacro tap (first form)
  "Tap for the threading macro.  Insert first into the first position in form,
then return first."
  `(progn
     (-> ,first ,form)
     ,first))

(defmacro rmapcar (lst fn)
  "Mapcar with arguments reversed.  Allows use with threading macro."
  `(mapcar ,fn ,lst))

