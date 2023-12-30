(ql:quickload :omg)

(defpackage :tomo
  (:use cl omg omgui jscl omgdaemon omgwidgets))

(in-package :tomo)

(defparameter-f *noize-treshhold* 0.0)

(defclass-f idb-connection ()
  ((connection :initarg :connection
               :accessor connection)))

(defclass-f idb-object-store ()
  ((name :initarg :name
         :accessor name
         :initform nil)
   (conn :initarg :connection
         :accessor conn
         :initform nil)
   (key :initarg :key
        :accessor key
        :initform nil)
   (auto-increment :initarg :auto-increment
                   :accessor auto-increment
                   :initform nil)
   (trans :initarg :trans
          :initform nil
          :accessor trans)
   (stor :initarg :trans
         :initform nil
         :accessor stor)))

(defclass-f idb-transaction ()
  ((stores :initarg :stores
           :accessor stores
           :initform nil)
   (mode :initarg :mode
         :accessor mode
         :initform :readonly)
   (durability :initarg :durability
               :accessor durability
               :initform :default)
   (conn :initarg :connection
         :accessor conn
         :initform (error "Please, specify a connection for idb-transaction!"))
   (trans :accessor trans)))

(defmethod-f initialize-instance :after ((tr idb-transaction) &rest args)
  (setf (slot-value tr 'stores)
        (map 'vector #'jscl::lisp-to-js
             (let ((str (stores tr)))
               (if str
                   (if (stringp str)
                       (list str)
                       str)
                   (store-names (conn tr))))))
  (setf (slot-value tr 'trans)
        ((jscl::oget (connection (conn tr)) "transaction")
         (stores tr)
         (case (mode tr)
           (:readonly "readonly")
           (:readwrite "readwrite")
           (:readwriteflush "readwriteflush")
           (t (error (format nil "Invalid mode for idb-transaction: ~A, must be :readonly, :readwrite or :readwriteflush!" (mode tr)))))
         (make-js-object :|durability|
           (case (durability tr)
              (:default "default")
              (:strict "strict")
              (:relaxed "relaxed")
              (t (error (format nil "Invalid durability for idb-transaction: ~A, must be :default, :strict or :relaxed" (durability tr)))))))))

(defmethod-f close ((c idb-connection))
  ((jscl::oget (connection c) "close")))

(defmethod-f store-names ((c idb-connection))
  (jscl::oget (connection c) "objectStoreNames"))

(defmethod-f create ((s idb-object-store))
  ((jscl::oget (connection (conn s)) "createObjectStore")
   (name s)
   (apply #'make-js-object
     `(,@(if (key s) `(:|keyPath| ,(key s)))
       ,@(if (auto-increment s) `(:|autoIncrement| t))))))

(defmethod-f initialize-instance :after ((s idb-object-store) &rest args)
  (if (and (not (conn s)) (not (trans s)))
      (error "Please specify a connection for idb-object-store!")
      (when (trans s)
        (setf (slot-value s 'conn) (conn (trans s)))
        (if (not (name s))
            (if (= 1 (length (stores (trans s))))
                (setf (slot-value s 'name) (jscl::js-to-lisp (aref (stores (trans s)) 0)))
                (error "Please, specify a name for idb-object-store! The transaction belongs to multiple stores!")))
        (setf (slot-value s 'stor) ((jscl::oget (trans (trans s)) "objectStore") (jscl::lisp-to-js (name s))))
        (setf (slot-value s 'key) (jscl::oget (stor s) "keyPath"))))
  (if (not (name s))
      (error "Please, specify a name for idb-object-store!"))
  (loop for name across (store-names (conn s)) when (equal (jscl::js-to-lisp name) (name s)) return t
    finally (setf (slot-value s 'stor) (create s))))

(defmethod-f add ((s idb-object-store) key data &key when-ok when-err put)
  (let* ((len (omgui::store-to-buffer data (jscl::make-new (winref "SharedArrayBuffer") 1)))
         (buf (jscl::make-new (winref "ArrayBuffer") len)))
    (omgui::store-to-buffer data buf)
    (let ((req ((jscl::oget (stor s) (if put "put" "add")) buf key)))
      (if when-ok (setf (jscl::oget req "onsuccess") (lambda (ev) (funcall when-ok))))
      (setf (jscl::oget req "onerror")
            (lambda (ev)
               (if when-err
                   (funcall when-err (jscl::oget req "error" "message"))
                   (error (format nil "IDB add error: ~A" (jscl::oget req "error" "message")))))))))

(defmethod-f get ((s idb-object-store) key cb &optional err)
  (let ((req ((jscl::oget (stor s) "get") key)))
    (setf (jscl::oget req "onsuccess")
          (lambda (ev)
            (let ((res (jscl::oget req "result")))
              (funcall cb (if res (omgui::load-from-buffer res) nil)))))
    (setf (jscl::oget req "onerror")
          (lambda (ev)
            (if err
                (funcall err (jscl::oget req "error" "message"))
                (error (format nil "idb get error: ~A" (jscl::oget req "error" "message"))))))))

(defmethod-f idb-delete ((s idb-object-store) key &key when-ok when-err)
  (let ((req ((jscl::oget (stor s) "delete") key)))
    (if when-ok (setf (jscl::oget req "onsuccess") (lambda (ev) (funcall when-ok))))
    (setf (jscl::oget req "onerror")
          (lambda (ev)
             (if when-err
                 (funcall when-err (jscl::oget req "error" "message"))
                 (error (format nil "IDB delete error: ~A" (jscl::oget req "error" "message"))))))))


(defmethod-f get-all-keys ((s idb-object-store) cb &optional err)
  (let ((req ((jscl::oget (stor s) "getAllKeys"))))
    (setf (jscl::oget req "onsuccess")
          (lambda (ev)
            (funcall cb (map 'vector #'jscl::js-to-lisp (jscl::oget req "result")))))
    (setf (jscl::oget req "onerror")
          (lambda (ev)
            (if err
                (funcall err (jscl::oget req "error" "message"))
                (error (format nil "get-all-keys error: ~A" (jscl::oget req "error" "message"))))))))

(def-local-macro-f with-indexed-db (conn-name-version &rest code)
  (let* ((conn (car conn-name-version))
         (name (cadr conn-name-version))
         (version (caddr conn-name-version))
         (version (if version version 1))
         (req (gensym))
         (err (gensym))
         (ev (gensym)))
    `(let ((,req ((jscl::oget (winref "indexedDB") "open") ,name ,version))
           (,err nil))
       (setf (jscl::oget ,req "onerror")
             (lambda (,ev)
               (error (format nil "Error when opening database ~A: ~A"
                        ,name
                        (jscl::oget ,req "error" "message")))))
       (setf (jscl::oget ,req "onsuccess")
             (lambda (,ev)
               (if (not ,err)
                   (let ((,conn (make-instance 'idb-connection :connection (jscl::oget ,req "result"))))
                     ,@code
                     (close ,conn)))))
       (setf (jscl::oget ,req "onupgradeneeded")
             (lambda (,ev)
               (setf ,err t)
               (if (= ,version 1)
                   (format t "Database ~A does not exists" ,name)
                   (format t "Database upgrade not yet implemented"))
               ((jscl::oget (jscl::oget ,req "result") "close"))))
       ,req)))

(def-local-macro-f setup-indexed-db (conn-name-version &rest code)
  (let* ((conn (car conn-name-version))
         (name (cadr conn-name-version))
         (version (caddr conn-name-version))
         (version (if version version 1))
         (req (gensym))
         (err (gensym))
         (ev (gensym)))
    `(let ((,req ((jscl::oget (winref "indexedDB") "open") ,name ,version))
           (,err nil))
       (setf (jscl::oget ,req "onerror")
             (lambda (,ev)
               (error (format nil "Error when opening database ~A: ~A"
                        ,name
                        (jscl::oget ,req "error" "message")))))
       (setf (jscl::oget ,req "onsuccess")
             (lambda (,ev)
               (let ((,conn (jscl::oget ,req "result")))
                 ((jscl::oget ,conn "close")))))
       (setf (jscl::oget ,req "onupgradeneeded")
             (lambda (,ev)
               (let ((,conn (make-instance 'idb-connection :connection (jscl::oget ,req "result"))))
                 ,@code))))))

(defun-f indexed-db-add (db store key val &key when-ok when-err)
  (with-indexed-db (conn db)
    (let* ((tr (make-instance 'idb-transaction :connection conn :stores store :mode :readwrite))
           (st (make-instance 'idb-object-store :trans tr)))
      (add st key val :when-ok when-ok :when-err when-err))))

(defun-f indexed-db-put (db store key val &key when-ok when-err)
  (with-indexed-db (conn db)
    (let* ((tr (make-instance 'idb-transaction :connection conn :stores store :mode :readwrite))
           (st (make-instance 'idb-object-store :trans tr)))
      (add st key val :when-ok when-ok :when-err when-err :put t))))


(def-local-macro-f indexed-db-get (val-db-store-key &rest code)
  (let* ((conn (gensym))
         (tr (gensym))
         (st (gensym))
         (val (car val-db-store-key))
         (db-store-key (cadr val-db-store-key))
         (db (car db-store-key))
         (store (cadr db-store-key))
         (key (caddr db-store-key)))
    `(with-indexed-db (,conn ,db)
       (let* ((,tr (make-instance 'idb-transaction :connection ,conn :stores ,store :mode :readonly))
              (,st (make-instance 'idb-object-store :trans ,tr)))
         (get ,st ,key (lambda (,val) ,@code))))))

(def-local-macro-f indexed-db-delete (db-store-key &rest code)
  (let* ((conn (gensym))
         (tr (gensym))
         (st (gensym))
         (db (car db-store-key))
         (store (cadr db-store-key))
         (key (caddr db-store-key)))
    `(with-indexed-db (,conn ,db)
       (let* ((,tr (make-instance 'idb-transaction :connection ,conn :stores ,store :mode :readwrite))
              (,st (make-instance 'idb-object-store :trans ,tr)))
         (idb-delete ,st ,key :when-ok (lambda () ,@code))))))

(def-local-macro-f indexed-db-get-all-keys (val-db-store &rest code)
  (let* ((conn (gensym))
         (tr (gensym))
         (st (gensym))
         (val (car val-db-store))
         (db-store (cadr val-db-store))
         (db (car db-store))
         (store (cadr db-store)))
    `(with-indexed-db (,conn ,db)
       (let* ((,tr (make-instance 'idb-transaction :connection ,conn :stores ,store :mode :readonly))
              (,st (make-instance 'idb-object-store :trans ,tr)))
         (get-all-keys ,st (lambda (,val) ,@code))))))

(def-local-macro-f if-idb-key (db-store-key true &optional false)
  (let* ((conn (gensym))
         (tr (gensym))
         (st (gensym))
         (val (gensym))
         (db (car db-store-key))
         (store (cadr db-store-key))
         (key (caddr db-store-key)))
    `(with-indexed-db (,conn ,db)
       (let* ((,tr (make-instance 'idb-transaction :connection ,conn :stores ,store :mode :readonly))
              (,st (make-instance 'idb-object-store :trans ,tr)))
         (get ,st ,key
           (lambda (,val)
             (if ,val ,true ,false)))))))

(defun-f set-db ()
  (setup-indexed-db (db "TestDB")
    (make-instance 'idb-object-store :connection db :name "tbl1"))
  nil)

(defun-f test-add ()
  (indexed-db-add "TestDB" "tbl1" "my-key2" "sdrthdsfhdsfghdfg"
    :when-ok (lambda () (jslog "ADD OK!"))
    :when-err (lambda (err) (jslog "ADD ERROR:" err))))

(defun-f test-get ()
  (indexed-db-get (val ("TestDB" "tbl1" "my-key"))
    (jslog val)))

(defun-f test-get-keys ()
  (indexed-db-get-all-keys (val ("TestDB" "tbl1"))
    (jslog (position "k1" val :test #'equal))))

(defun-f test-if ()
  (if-idb-key ("TestDB" "tbl1" "my-key")
    (jslog "KEY EXISTS")
    (jslog "KEY DOES NOT EXISTS")))


(load "macros.lisp")
(load "settings.lisp")
(load "trail.lisp")
(load "profiles.lisp")
(load "art-solver.lisp")
(load "sample-src.lisp")
(load "real-src.lisp")
(load "app.lisp")
