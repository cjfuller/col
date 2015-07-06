(in-package :cl-user)

(defpackage :col.mapping
  (:documentation "A set of consistent interfaces for dealing with different mapping types.")
  (:use :cl)
  (:export
   :alist-p
   :plist-p
   :remove-from-plist
   :mget
   :mget*
   :hset
   :htcopy
   :mset
   :mset!
   :mset*
   :mset&
   :ht
   :plist->alist
   :alist->ht
   :keys))

(in-package :col.mapping)

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

(defmethod mget ((mapping fset:map) key)
  (fset:lookup mapping key))

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
  (let ((newhash (make-hash-table
                  :test (hash-table-test hash)
                  :size (hash-table-size hash)
                  :rehash-size (hash-table-rehash-size hash)
                  :rehash-threshold (hash-table-rehash-threshold hash))))
    (loop for key being the hash-keys of hash
            using (hash-value value)
          do (hset newhash key value))
    newhash))

(defgeneric mset! (obj key value)
  (:documentation "Set the value associated with a key in a mapping in-place."))

(defmethod mset! ((obj hash-table) key value)
  (hset obj key value))

(defmethod mset! ((obj list) key value)
  ;; Fail if it's not an alist
  (unless (listp (car obj))
    (error "Can only use mset! on alists."))
  (if (assoc key obj)
      (setf (cdr (assoc key obj)) value)
      (nconc obj (list (cons key value)))))

(defmethod mset! ((obj fset:map) key value)
  (error "FSet maps are immutable."))

(defgeneric mset (obj key value)
  (:documentation "Return a mapping with value associated with key.  Don't modify the original mapping."))

(defmethod mset ((obj list) key value)
  (if (alist-p obj)
      (acons key value obj)
      (cons key (cons value obj))))

(defmethod mset ((obj hash-table) key value)
  (let ((newtable (htcopy obj)))
    (hset newtable key value)
    newtable))

(defmethod mset ((obj fset:map) key value)
  (fset:with obj key value))

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

(defmacro mset& (place key value)
  "Mset and reassign to the provided place.  Return the new value assigned to place."
  `(progn (setf ,place (mset ,place ,key ,value))
          ,place))

(defgeneric keys (obj)
  (:documentation "Get the keys from a mapping (alist, plist, or hash table)."))

(defmethod keys ((h hash-table))
  (loop for key being the hash-keys of h collect key))

(defmethod keys ((l list))
  (if (alist-p l)
      (mapcar #'car l)
      (loop for i in l by #'cddr collect i)))

(defmethod keys ((m fset:map))
  (fset:domain m))

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
