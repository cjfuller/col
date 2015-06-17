(in-package :cl-user)

(defpackage :cjf-stdlib
  (:use :cl :split-sequence)
  (:export :mget :mget* :println :-> :->string :keys :ht :plist->alist :hset
   :slurp :alist->ht :<- :mset! :remove-from-plist :mset :<> :tee :rmapcar :tap
   :mset*
   ; re-export from split-sequence
   :split-sequence :split-sequence-if :split-sequence-if-not
   ))

(in-package :cjf-stdlib)

(defun alist-p (mapping)
  "Shortcut for determining if a mapping is an alist, given that it must be one
of an alist, plist, or hash."
  (and (listp mapping)
       (listp (car mapping))))

(defun plist-p (mapping)
  "Shortcut for determining if a mapping is a plist, given that it must be one
of an alist, plist, or hash."
  (and (listp mapping)
       (symbolp (car mapping))))

(defun remove-from-plist (plist k)
  "Return a new plist with the specified key and its value removed."
  (loop for x in plist by #'cddr
        for y in (cdr plist) by #'cddr
        unless (eql k x)
          collect x
        unless (eql k x)
          collect y))

(defgeneric mget (mapping key)
  (:documentation "Get the value associated with a key in a mapping."))

(defmethod mget ((mapping list) key)
  (if (alist-p mapping)
      (cdr (assoc key mapping :test #'equal))
      (getf mapping key)))

(defmethod mget ((mapping hash-table) key)
  (gethash key mapping))

(defmethod mget (mapping key)
  (slot-value mapping key))

(defun mget* (mapping &rest keys)
  "Like mget, but takes a series of keys for dealing with nested mappings.
The first key corresponds to the outermost layer of mapping."
  (if (cdr keys)
      (apply #'mget* (mget mapping (car keys)) (cdr keys))
      (mget mapping (car keys))))

(defun hset (hash key value)
  "A slightly more convenient shorthand for setting values in a hash table."
  (setf (gethash key hash) value))

(defun htcopy (hash)
  "Copy a hash table."
  (:= newhash (make-hash-table
               :test (hash-table-test hash)
               :size (hash-table-size hash)
               :rehash-size (hash-table-rehash-size hash)
               :rehash-threshold (hash-table-rehash-threshold hash))
    (loop for key being the hash-keys of hash
            using (hash-value value)
          do (hset newhash key value))
    newhash))

(defgeneric mset! (obj key value)
  (:documentation "Set the value associated with a key in a mapping in-place."))

(defmethod mset! ((obj hash-table) key value)
  (hset h key value))

(defmethod mset! ((obj list) key value)
  ;; Fail if it's not an alist
  (unless (listp (car obj))
    (error "Can only use mset! on alists."))
  (if (assoc key obj)
      (setf (cdr (assoc key obj)) value)
      (nconc obj (list (cons key value)))))


(defgeneric mset (obj key value)
  (:documentation "Return a mapping with value associated with key.  Don't modify the original mapping."))

(defmethod mset ((obj list) key value)
  (if (alist-p obj)
      (acons key value obj)
      (cons key (cons value obj))))

(defmethod mset ((obj hash-table) key value)
  (:= newtable (htcopy obj)
    (hset newtable key value)
    newtable))

(defun mset* (obj keylist value)
  "Return a mapping where the value at the keys described by
  keylist is set to values.  Don't modify any of the nested mappings.

  If any of the keys are missing or have a nil value, a nested mapping (an
  alist by default, may change in the future) is inserted so that the final
  object has the full series of keys pointing to the value.
"
  (if (cdr keylist)
      (mset obj (car keylist)
            (mset* (mget obj (car keylist)) (cdr keylist) value))
      (mset obj (car keylist) value)))

(defgeneric ->string (obj)
  (:documentation "Convert an object to a string."))

(defmethod ->string (obj)
  obj)

(defun println (s)
  "Princ, followed by a newline.

Calls ->string on the object passed in.  (Default implementation is the
identity function.)
"
  (princ (->string s))
  (terpri))

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

(defgeneric keys (obj)
  (:documentation "Get the keys from a mapping (alist, plist, or hash table)."))

(defmethod keys ((h hash-table))
  (loop for key being the hash-keys of h collect key))

(defmethod keys ((l list))
  (if (alist-p l)
      (mapcar #'car l)
      (loop for i in l by #'cddr collect i)))

;; TODO: equality test, etc.
(defun ht (&rest plist)
  (let ((h (make-hash-table)))
    (mapc (lambda (k) (setf (gethash k h) (mget plist k))) (keys plist))
    h))

(defun plist->alist (plist)
  (loop for x in plist by #'cddr
        for y in (cdr plist) by #'cddr
        collect (cons x y)))

(defun alist->ht (alist &rest hash-table-initargs)
  (apply #'alexandria:alist-hash-table (cons alist hash-table-initargs)))

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

Currently defaults to returning a hash table with test #'equal.  This will
change in the future.

Examples:
#M{:hello :world}
"
  (declare (ignore char))
  ; Next character could be a ( in which case we want to read options
  ; or a { in which case we want to read the elements
  (let ((opts nil)
        (elts nil)
        (output-type :hash-table)
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

    ;; TODO: this is going to be inefficient-- accumulating elements into a
    ;; list, then reaccumulating them into whatever output structure was
    ;; requested.  Pass a flag so that we can construct the correct
    ;; representation directly.
    (setq elts (map-elements-reader stream))
    ;; TODO: implement opts for what type of object to return
    ;; NOTE: in the future, a functional immutable map will be the default
    (case output-type
      (:hash-table `(apply #'alexandria:plist-hash-table (cons (list ,@elts) (list ,@opts))))
      (:alist `(plist->alist (list ,@elts)))
      (:plist `(list ,@elts)))))

;; TODO: make this enableable
(set-dispatch-macro-character #\# #\M #'map-literal-reader)

(defun shell-command-reader (stream char arg)
  (let ((str (cl-interpol::interpol-reader stream char arg)))
    `(inferior-shell:run/ss ,str)))

;; TODO: make this enableable
(set-dispatch-macro-character #\# #\! #'shell-command-reader)

(defun slurp (fn &key (lines nil))
  "Slurp a file into a strng.

If lines is non-nil, return a list, one string per line (newlines stripped)."
  (with-open-file (f fn)
    (if lines
        (uiop:slurp-stream-lines f)
        (uiop:slurp-stream-string f))))

(defmacro := (var val &body body)
  "Shorthand for let for a single binding."
  `(let ((,var ,val))
     ,@body))

(defun <> (&rest strings)
  "Shorthand for concatenating strings."
  (apply #'concatenate (cons 'string strings)))
