(in-package :cl-user)

(defpackage :col.reader
  (:documentation "Reader extensions.
The shell command reader uses the syntax #!\"shell-command\" to run the command using inferior-shell and return stdout as a string.  Within the shell command, you can use interpolation syntax as for cl-interpol.

The map literal reader uses the syntax #M(opts-plist){key value key value} to generate mappings.  opts-plist is an optional (you can leave out the parens entirely) list of options controlling the return type of the mapping.  Currently supported options: :type (one of :alist :plist :hash-table), the type of the mapping that's created; :test, when a hash-table, the equality test for that hash table.  Default is an alist.
")
  (:use :cl :col.mapping)
  (:export
   :enable-map-literals
   :enable-shell-command-literals
   :enable-reader-exts
   :pop-reader-exts))

(in-package :col.reader)

(defvar *previous-readtables* nil)

(defun read-terminal-bracket (stream char)
  "Temporary reader for terminal bracket while reading a map literal.

This just pushes the bracket back on the stream and returns nothing, allowing
the map-elements-reader to handle the character.  (We just don't want the
regular reader to handle it.)

This ensures that things like:
#M{:hello :world}
don't try to read the closing bracket as part of the second keyword.

(Note that things like #M{:hello :|bracketed}world|} will still work.)
"
  (unread-char char stream)
  (values))

(defun map-elements-reader (stream &optional (acc nil))
    (case (peek-char t stream)
      (#\{ (read-char stream) (map-elements-reader stream acc))
      (#\} (read-char stream) (reverse acc))
      (otherwise
         (let ((*readtable* (copy-readtable *readtable*)))
           (set-macro-character #\} #'read-terminal-bracket)
           (map-elements-reader stream (cons (read stream t nil t) acc))))))

(defun map-literal-reader (stream char arg)
  "A map literal looks like #M(opts plist [optional]){key value key value}.

Defaults to returning an fset map.

Examples:
#M{:hello :world}
"
  (declare (ignore char))
  ; Next character could be a ( in which case we want to read options
  ; or a { in which case we want to read the elements
  (let ((opts nil)
        (elts nil)
        (output-type :fset-map)
        (next-char (peek-char nil stream)))
    (when (eq next-char #\()
      (setq opts (read stream t nil t)))
    ;; set up some default opts
    (unless (member :test opts)
      (setq opts (cons :test (cons '(function equal) opts))))

    ;; pull out reader opts that apply to this function instead of the
    ;; downstream options for the data structure
    (when (member :type opts)
      (setq output-type (mget opts :type))
      (setq opts (remove-from-plist opts :type)))

    ;; TODO: this is inefficient-- accumulating elements into a list, then
    ;; reaccumulating them into whatever output structure was requested.
    ;; Construct the correct representation directly instead.
    (setq elts (map-elements-reader stream))
    (case output-type
      (:hash-table `(apply #'alexandria:plist-hash-table (cons (list ,@elts) (list ,@opts))))
      (:fset-map `(fset:map ,@(loop for k in elts by #'cddr
                                    for v in (cdr elts) by #'cddr
                                    collect (list k v))))
      (:alist `(plist->alist (list ,@elts)))
      (:plist `(list ,@elts)))))

(defun shell-command-reader (stream char arg)
  (let ((str (cl-interpol::interpol-reader stream char arg)))
    `(inferior-shell:run/ss ,str)))

(defmacro enable-inner (do-push &body enable-forms)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (when ,do-push
       (push *readtable* *previous-readtables*)
       (setq *readtable* (copy-readtable)))
     ,@enable-forms))

(defmacro enable-reader-exts (&key (do-push t))
  "Enable shell command and map literal readers.  Also enable the cl-interpol reader."
  (enable-inner do-push
   (set-dispatch-macro-character #\# #\! #'shell-command-reader)
   (set-dispatch-macro-character #\# #\M #'map-literal-reader)
   (set-dispatch-macro-character #\# #\? #'cl-interpol::interpol-reader)))

(defmacro pop-reader-exts ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (when *previous-readtables*
       (setq *readtable* (pop *previous-readtables*)))))

(defmacro enable-map-literals (&key (do-push t))
  "Enable only the map literal reader."
  (enable-inner do-push
   (set-dispatch-macro-character #\# #\M #'map-literal-reader)))

(defmacro enable-shell-command-literals (&key (do-push t))
  "Enable only the shell command reader."
  (enable-inner do-push
   (set-dispatch-macro-character #\# #\! #'shell-command-reader)))
