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













;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ; (defclass-f sample-picker (graph)
; ;   ((matrix :accessor matrix)
; ;    (orig-plot :accessor orig-plot)
; ;    (blured-plot :accessor blured-plot)
; ;    (sample-char :accessor sample-char
; ;                 :initform "A")
; ;    (sample-div-lambda :accessor sample-div-lambda)
; ;    (phase :initform nil
; ;           :accessor phase)
; ;    (preserve-aspect-ratio :initform t)))
;
; (defclass-f sample-profile (profile)
;   ((plot :initarg plot
;          :accessor plot)
;    (fplot :initarg fplot
;           :accessor fplot)))
;
; (defclass-f profile-source ()
;   ((profiles :accessor profiles
;              :initform (list))
;    (trail :initarg :trail
;           :initform nil
;           :accessor trail)
;    (fourier-matrix :initform nil)
;    (max-d :initform nil)
;    (current-matrix :initform nil)
;    (current-chi :initform nil)
;    (solver-worker :initform nil)))
;
;
; (defclass-f trail-fplot (tabular-plot)
;   ((phase :initarg :phase
;           :accessor phase)
;    (color :initform "green"
;           :accessor color)))
;
; (defclass-f fourier-matrix-plot (matrix-plot)
;   ((matrix :initform nil)
;    (palette :initarg :palette
;             :initform (lambda (x) (list (sqrt x) (sqrt x) (sqrt x)))
;             :accessor palette)
;    (norm :initarg :norm
;          :initform t
;          :accessor norm)))
;
; (defclass-f rev-fourier-matrix-plot (matrix-plot)
;   ((matrix :initform nil)
;    (norm :initarg :norm
;          :initform t
;          :accessor norm)))
;
; (defclass-f rev-fourier-graph (graph)
;   ((xcaption :initform (create-element "span" :|style.whiteSpace| "nowrap" :|innerHTML| "Vx (km/s)")
;              :accessor xcaption)
;    (ycaption :initform (create-element "span" :|style.whiteSpace| "nowrap" :|innerHTML| "Vy (km/s)")
;              :accessor ycaption)
;    (show-scales :initform '(:left :right :top :bottom))
;    (matrix :initarg :matrix
;            :initform nil)
;    (mousein :initform nil
;             :accessor mousein)
;    (source :accessor source
;            :initarg :source
;            :initform (error "Source must be provided for fourier-graph"))
;    (preserve-aspect-ratio :initform t)))
;
;
;
; (defclass-f fourier-graph (graph)
;   ((xcaption :initform ""
;              :accessor xcaption)
;    (ycaption :initform ""
;              :accessor ycaption)
;    (xmin :initform -1)
;    (xmax :initform 1)
;    (ymin :initform -1)
;    (ymax :initform 1)
;    (show-scales :initform '())
;    (matrix :initarg :matrix
;            :initform nil)
;    (source :accessor source
;            :initarg :source
;            :initform (error "Source must be provided for fourier-graph"))))
;
; (defclass-f fourier-block (omg-widget)
;   ((fourier-graph :accessor fourier-graph)
;    (rev-fourier-graph :accessor rev-fourier-graph)
;    (informer :accessor informer)
;    (source :accessor source
;            :initarg :source
;            :initform (error "Source must be provided for fourier-block"))))
;
; ; (defclass-f main-view (omg-widget)
; ;   ((trail :initform (make-instance 'trail-graph)
; ;           :accessor trail)
; ;    (sample-src :initform nil
; ;                :accessor sample-src)
; ;    (fourier-block :accessor fourier-block)
; ;    (psf-graph :initform (make-instance 'psf-graph)
; ;               :accessor psf-graph)
; ;    (root :initform nil
; ;          :accessor root)))
;
; (defclass-f informer (omg-widget)
;   ((source :accessor source
;            :initarg :source
;            :initform (error "Source must be specified for informer"))))
;
;
; (defparameter-f *cur-step* 0)
;
;
; (defmethod-f reblur ((g sample-picker))
;   (if (slot-boundp g 'blured-plot) (remove-plot (blured-plot g)))
;   (let* ((nx (resolution g))
;          (ny nx)
;          (odat (matrix g)))
;     (bind-exit-values-for (bdat)
;       (run-in-web-worker nil
;         (cache-vars t)
;         (let* ((a (sqr (* 2 (/ (jsln 2) *cur-fwhm*))))
;                (cf (* (/ 2 *cur-fwhm*) (sqrt (/ (jsln 2) pi))))
;                (bdat (make-array (list nx ny)))
;                (rd (loop for i below nx when
;                      (let* ((x (/ i (- nx 1)))
;                             (v (exp (* -1 a (* (sqr (* *max-v* x)))))))
;                        (< v 0.05))
;                      return i)))
;           (loop for i below nx do
;             (let ((x (- (* 2.0 (/ i (- nx 1))) 1.0)))
;               (loop for j below ny do
;                 (let ((y (- (* 2.0 (/ j (- ny 1))) 1.0)))
;                   (setf (aref bdat (+ i (* nx j)))
;                         (loop for i1 from (max 0 (- i rd)) below (min nx (+ i rd)) sum
;                           (let ((x1 (- (* 2.0 (/ i1 (- nx 1))) 1.0)))
;                             (loop for j1 from (max 0 (- j rd)) below (min ny (+ j rd)) sum
;                               (let ((y1 (- (* 2.0 (/ j1 (- ny 1))) 1.0)))
;                                 (* (aref odat (+ i1 (* j1 nx)))
;                                    cf
;                                    (exp (* -1 a (* (sqr *max-v*) (+ (sqr (- x x1)) (sqr (- y y1))))))))))))))))
;           bdat))
;       (add-plot g (setf (slot-value g 'blured-plot) (make-instance 'matrix-plot :matrix bdat :norm t)))
;       (ensure-element (root g)
;         (setf (jscl::oget (root (blured-plot g)) "style" "visibility") "hidden")))))
;
; (defmethod-f make-sample-div ((s sample-picker))
;   (funcall (sample-div-lambda s)))
;
; ; (defmethod-f render-widget :around ((g sample-picker))
; ;   (let* ((root (call-next-method))
; ;          (nx (resolution g))
; ;          (ny (resolution g))
; ;          (ch (sample-char g))
; ;          (cnv1 (create-element "canvas" :|width| nx
; ;                                         :|height| ny))
; ;          (ctx1 ((jscl::oget cnv1 "getContext") "2d")))
; ;     (setf (jscl::oget ctx1 "font") (format nil "~Apx serif" nx))
; ;     (let* ((m ((jscl::oget ctx1 "measureText") ch))
; ;            (mw (jscl::oget m "width"))
; ;            (mh (jscl::oget m "actualBoundingBoxAscent")))
; ;       (setf (jscl::oget ctx1 "fillStyle") "black")
; ;       ((jscl::oget ctx1 "fillText") ch (/ (- nx mw) 2) (- ny (/ (- ny mh) 2)))
; ;       (let ((idat (jscl::oget ((jscl::oget ctx1 "getImageData") 0 0 nx ny) "data"))
; ;             (odat (make-array (list nx ny)))
; ;             (phi-div (create-element "div" :|style.position| "absolute"
; ;                                            :|style.left| 0
; ;                                            :|style.top| 0
; ;                                            :|style.background| "yellow"
; ;                                            :|style.fontSize| "0.75em"
; ;                                            :|style.width| "3.5em"
; ;                                            :|innerHTML| "&#966;=0"
; ;                                            :|style.zIndex| 20
; ;                                            :|style.margin| "5px"
; ;                                            :|style.visibility| "hidden"))
; ;             (arrow (make-svg :|viewBox| "0 0 400 400"
; ;                              '(:line :|x1| 200 :|y1| 600
; ;                                      :|x2| 200 :|y2| -200
; ;                                      :|stroke| "white"
; ;                                      :|opacity| "0.5"
; ;                                      :|stroke-width| 5)
; ;                              '(:line :|x1| 190 :|y1| 230
; ;                                      :|x2| 200 :|y2| 200
; ;                                      :|stroke| "white"
; ;                                      :|opacity| "0.5"
; ;                                      :|stroke-width| 5)
; ;                              '(:line :|x1| 210 :|y1| 230
; ;                                      :|x2| 200 :|y2| 200
; ;                                      :|stroke| "white"
; ;                                      :|opacity| "0.5"
; ;                                      :|stroke-width| 5)
; ;                              '(:line :|x1| 200 :|y1| 600
; ;                                      :|x2| 200 :|y2| -200
; ;                                      :|stroke| "red"
; ;                                      :|stroke-width| 2
; ;                                      :|stroke-dasharray| 10)
; ;                              '(:line :|x1| 190 :|y1| 230
; ;                                      :|x2| 200 :|y2| 200
; ;                                      :|stroke| "red"
; ;                                      :|stroke-width| 2)
; ;                              '(:line :|x1| 210 :|y1| 230
; ;                                      :|x2| 200 :|y2| 200
; ;                                      :|stroke| "red"
; ;                                      :|stroke-width| 2)
; ;                              :|style.position| "absolute"
; ;                              :|style.left| 0
; ;                              :|style.top| 0
; ;                              :|style.overflow| "visible"
; ;                              :|style.zIndex| 10
; ;                              :|style.visibility| "hidden")))
; ;         (setf (slot-value g 'sample-div-lambda)
; ;               (lambda ()
; ;                 (let ((a-div (make-svg :|viewBox| "0 0 500 500"
; ;                                  :|style.position| "absolute"
; ;                                  :|style.width| "100%"
; ;                                  :|style.height| "100%"
; ;                                  :|style.left| "0"
; ;                                  :|style.top| "0"
; ;                                  :|style.zIndex| 18
; ;                                  :|preserveAspectRatio| "none"
; ;                                `(text :|fill| "none"
; ;                                       :|stroke| "red"
; ;                                       :|stroke-width| "3px"
; ;                                       :|stroke-linejoin| "round"
; ;                                       :|style.fontSize| "500"
; ;                                       :|x| ,(* 500 (/ (- nx mw) (* 2 nx)))
; ;                                       :|y| ,(* 500 (- 1 (/ (- ny mh) (* 2 ny))))
; ;                                       ,ch))))
; ;                   (ensure-element a-div
; ;                     (setf (jscl::oget a-div "style" "fontSize")
; ;                           (jscl::oget (parent-element a-div) "clientHeight")))
; ;
; ;                   a-div)))
; ;         (loop for i below nx do
; ;           (loop for j below ny do
; ;             (setf (aref odat (+ i (* nx j)))
; ;                   (/ (aref idat (+ 3 (* 4 (+ i (* nx j))))) 255.0))))
; ;         (setf (slot-value g 'matrix) odat)
; ;         (reblur g)
; ;         (add-plot g (setf (slot-value g 'orig-plot) (make-instance 'matrix-plot :matrix odat :norm t)))
; ;         (let ((sd (make-sample-div g)))
; ;           (append-element sd (graph g))
; ;           (append-element arrow (graph g))
; ;           (append-element phi-div (graph g))
; ;           (setf (jscl::oget sd "style" "visibility") "hidden")
; ;           (setf (jscl::oget (root g) "onmouseleave")
; ;                 (lambda (ev)
; ;                   (setf (jscl::oget arrow "style" "visibility") "hidden")
; ;                   (setf (jscl::oget phi-div "style" "visibility") "hidden")
; ;                   (if (slot-boundp g 'blured-plot) (setf (jscl::oget (root (blured-plot g)) "style" "visibility") "hidden"))
; ;                   (setf (jscl::oget (root (orig-plot g)) "style" "visibility") "visible")
; ;                   (setf (jscl::oget sd "style" "visibility") "hidden")))
; ;           (setf (jscl::oget (root g) "onmousemove")
; ;                 (lambda (ev)
; ;                   (setf (jscl::oget arrow "style" "visibility") "visible")
; ;                   (setf (jscl::oget phi-div "style" "visibility") "visible")
; ;                   (if (slot-boundp g 'blured-plot) (setf (jscl::oget (root (blured-plot g)) "style" "visibility") "visible"))
; ;                   (setf (jscl::oget sd "style" "visibility") "visible")
; ;                   (setf (jscl::oget (root (orig-plot g)) "style" "visibility") "hidden")
; ;                   (let* ((x (+ (xmin g)
; ;                                (* (/ (- (jscl::oget ev "pageX") (jscl::oget (root g) "offsetLeft"))
; ;                                      (jscl::oget (root g) "clientWidth"))
; ;                                   (- (xmax g) (xmin g)))))
; ;                          (y (+ (ymin g)
; ;                                (* (- 1
; ;                                      (/ (- (jscl::oget ev "pageY") (jscl::oget (root g) "offsetTop"))
; ;                                         (jscl::oget (root g) "clientHeight")))
; ;                                   (- (ymax g) (ymin g)))))
; ;                          (angle (jsatan2 y x))
; ;                          (phase (+ 0.75 (/ angle (* 2 pi))))
; ;                          (phase (if (> phase 1) (- phase 1) phase)))
; ;                     (setf (slot-value g 'phase) phase)
; ;                     (setf (jscl::oget arrow "style" "rotate")
; ;                           (format nil "~Arad" (- (* 0.5 pi) angle)))
; ;                     (setf (jscl::oget phi-div "innerHTML")
; ;                           (subseq
; ;                             (format nil "&#966;=~A000000000000" (* 0.001 (truncate (* 1000.0 phase))))
; ;                             0 12))))))))
; ;     root))
;
; (defun-r refresh-me (id)
;   (thread-in-session
;     (sleep 0.1)
;     (remote-exec `(refresh-cb ',id) t))
;   nil)
;
; (defparameter-f *refresh-cbs* nil)
;
; (defun-f refresh-cb (id)
;   (funcall (gethash id *refresh-cbs*))
;   (remhash id *refresh-cbs*))
;
; (defun-f refresh (cb)
;   (if (not *refresh-cbs*) (setf *refresh-cbs* (make-hash-table)))
;   (let* ((*package* (find-package :tomo))
;          (key (gensym2)))
;     (setf (gethash key *refresh-cbs*) cb)
;     (refresh-me key)))
;
; ; (defmethod-f render-widget ((s sample-profile-source))
; ;   (setf (slot-value s 'picker) (make-instance 'sample-picker))
; ;   (setf (slot-value s 'root)
; ;         (create-element "div" :|style.float| "left"
; ;                               :|style.width| "100%"
; ;           :append-element
; ;             (create-element "div"
; ;                             :|style.width| "100%"
; ;               :append-element (render-widget (picker s)))
; ;           :append-element
; ;             (let ((inp (create-element "input" :|type| "text"
; ;                                                :|value| "21"
; ;                                                :|size| "5"
; ;                                                :|style.marginLeft| "1em"
; ;                                                :|style.marginRight| "1em"
; ;                                                :|style.textAlign| "center")))
; ;               (create-element "div" :|style.textAlign| "center"
; ;                                     :|style.marginTop| "3.5em"
; ;                 :append-element "Take"
; ;                 :append-element inp
; ;                 :append-element (create-element "button"
; ;                                   :|onclick| (lambda (ev)
; ;                                                (remove-all-profiles s)
; ;                                                (let* ((n (floor (js-parse-float (jscl::oget inp "value"))))
; ;                                                       (d (/ 1.0 n))
; ;                                                       (ntasks (jscl::oget (winref "navigator") "hardwareConcurrency"))
; ;                                                       (workers (loop for i below ntasks collect (make-instance 'classic-worker :persistent-cache t)))
; ;                                                       (done-flags (loop for i below n collect nil))
; ;                                                       (pb (make-instance 'get-prof-progress
; ;                                                             :stop-cb (lambda (pb)
; ;                                                                        (map nil #'kill workers)
; ;                                                                        (close pb)))))
; ;                                                  (append-element (render-widget pb))
; ;                                                  (multiple-value-bind (x rem) (floor 0.5 d)
; ;                                                    (let ((p (picker s))
; ;                                                          (d (/ 1.0 (+ n (if (= 0 rem) 0.5 0)))))
; ;                                                      (loop for i below n do
; ;                                                        (multiple-value-bind (x nw) (floor i ntasks)
; ;                                                          (let* ((i i)
; ;                                                                 (ph (* i d))
; ;                                                                 (ww (nth nw workers)))
; ;                                                            (when-worker-free ww
; ;                                                              (bind-exit-values-for (prof)
; ;                                                                (run-in-web-worker ww
; ;                                                                  (cache-vars t)
; ;                                                                  (let ((p p))
; ;                                                                    (get-profile p ph)))
; ;                                                                (progn
; ;                                                                  (add-profile s prof)
; ;                                                                  (setf (nth i done-flags) t)
; ;                                                                  (set-progress (bar pb) (/ (count-if (complement #'null) done-flags)
; ;                                                                                            (length done-flags)))
; ;                                                                  (if (notany #'null done-flags)
; ;                                                                      (progn
; ;                                                                        (map nil #'kill workers)
; ;                                                                        (close pb)))))))))))))
; ;                                   :|innerHTML| "profiles")))
; ;           :append-element
; ;             (create-element "div" :|style.textAlign| "center"
; ;                                   :|style.marginTop| "1.5em"
; ;               :append-element
; ;                 (create-element "button" :|onclick| (lambda (ev)
; ;                                                       (remove-all-profiles s))
; ;                                          :|style.width| "100%"
; ;                                          :|innerHTML| "clear all profiles"))
; ;           :append-element
; ;             (create-element "div" :|style.textAlign| "center"
; ;                                   :|style.marginTop| "1.5em"
; ;               :append-element
; ;                 (create-element "button" :|onclick| (lambda (ev)
; ;                                                       (let* ((lnk (create-element "a"))
; ;                                                              (data (apply #'concatenate
; ;                                                                       (cons 'string
; ;                                                                         (mapcar (lambda (p)
; ;                                                                                   (format nil "~A~%~A~%"
; ;                                                                                     (phase p)
; ;                                                                                     (apply #'concatenate
; ;                                                                                        (cons 'string
; ;                                                                                          (mapcar (lambda (d)
; ;                                                                                                    (format nil "~A ~A~%" (car d) (cdr d)))
; ;                                                                                                  (data p))))))
; ;                                                                                 (profiles s)))))
; ;                                                              (blb (jscl::make-new (winref "Blob")
; ;                                                                     (make-array '(1) :initial-element (jscl::lisp-to-js data))
; ;                                                                     (make-js-object :|type| "text/plain"))))
; ;                                                         (setf (jscl::oget lnk "download") "sample.dat")
; ;                                                         (setf (jscl::oget lnk "href")
; ;                                                               ((jscl::oget (jscl::lisp-to-js (jscl::%js-vref "URL")) "createObjectURL") blb))
; ;                                                         ((jscl::oget lnk "click") ev)
; ;                                                         ((jscl::oget (jscl::lisp-to-js (jscl::%js-vref "URL")) "revokeObjectURL") (jscl::oget lnk "href"))))
; ;                                          :|style.width| "100%"
; ;                                          :|innerHTML| "save profiles"))))
; ;   (setf (jscl::oget (root (picker s)) "onclick")
; ;         (lambda (ev)
; ;           (bind-exit-values-for (prof)
; ;             (run-in-web-worker nil
; ;               (cache-vars t)
; ;               (let ((x s))
; ;                 ; (format t "AAA: ~A" s)
; ;                 (get-profile (picker x) (phase (picker x)))))
; ;             (add-profile s prof))))
; ;   (root s))
;
; (defparameter-f *current-delta-cf* 1000.0)
;
; (defmethod-f render-widget :around ((w rev-fourier-graph))
;   (call-next-method)
;   (if (typep (source w) (find-class 'sample-profile-source))
;       (let ((sd (make-sample-div (picker (source w)))))
;         (if (not (mousein w))
;             (setf (jscl::oget sd "style" "visibility") "hidden"))
;         (setf (jscl::oget (root w) "onmouseleave")
;               (lambda (ev)
;                 (setf (slot-value w 'mousein) nil)
;                 (setf (jscl::oget sd "style" "visibility") "hidden")))
;         (setf (jscl::oget (root w) "onmousemove")
;               (lambda (ev)
;                 (setf (slot-value w 'mousein) t)
;                 (setf (jscl::oget sd "style" "visibility") "visible")))
;         (append-element sd (graph w))))
;   (root w))
;
;
;
; (defmethod-f render-widget ((w fourier-block))
;   (setf (slot-value w 'root)
;         (let* ((calc-bar (make-instance 'progress-bar :bg-style '(:|style.border| "1px solid black"
;                                                                   :|style.background| "white"
;                                                                   :|style.visibility| "hidden")
;                                                       :width "100%")))
;           (create-element "div" :|style.width| "100%"
;             ; :append-element
;             ;   (create-element "div" :|style.padding| "3em"
;             ;                         :|style.width| "14em"
;             ;                         :|style.height| "14em"
;             ;     :append-element (render-widget (fourier-graph w)))
;             :append-element
;               (create-element "div" :|style.width| "100%"
;                 :append-element (render-widget (rev-fourier-graph w)))
;             :append-element
;               (create-element "span" :|style.padding| "0.5em"
;                                      :|style.width| "80%"
;                                      :|style.display| "inline-block"
;                 :append-element (render-widget calc-bar))
;             :append-element
;               (create-element "span" :|style.padding| "1em"
;                                      :|style.width| "80%"
;                                      :|style.display| "inline-block"
;                 :append-element (render-widget (setf (slot-value w 'informer) (make-instance 'informer :source (source w)))))
;             ; :append-element
;             ;   (create-element "span"
;             ;                   :|style.display| "inline-block"
;             ;                   :|style.width| "100%"
;             ;                   :|style.marginTop| "1em"
;             ;                   :|style.textAlign| "center"
;             ;     :append-element (create-element "button" :|innerHTML| "Update fourier image"
;             ;                       :|onclick| (lambda (ev)
;             ;                                    (remove-all-plots (fourier-graph w))
;             ;                                    (remove-all-plots (rev-fourier-graph w))
;             ;                                    (setf (slot-value (source w) 'fourier-matrix) nil)
;             ;                                    (setf (slot-value (source w) 'current-matrix) nil)
;             ;                                    (add-plot (fourier-graph w) (make-instance 'fourier-matrix-plot))
;             ;                                    (add-plot (rev-fourier-graph w) (make-instance 'rev-fourier-matrix-plot)))))
;             ; :append-element
;             ;   (create-element "span"
;             ;                   :|style.display| "inline-block"
;             ;                   :|style.width| "100%"
;             ;                   :|style.marginTop| "1em"
;             ;                   :|style.textAlign| "center"
;             ;     :append-element (create-element "button" :|innerHTML| "Set gray"
;             ;                       :|onclick| (lambda (ev)
;             ;                                    (let* ((nx (resolution (source w)))
;             ;                                           (m (make-array (list nx nx)))
;             ;                                           (sum (/ 1.0 (loop for i below nx sum
;             ;                                                         (loop for j below nx sum
;             ;                                                           (let ((x (- (* 2.0 (/ i (- nx 1))) 1.0))
;             ;                                                                 (y (- (* 2.0 (/ j (- nx 1))) 1.0)))
;             ;                                                             (setf (aref m (+ i (* nx j)))
;             ;                                                                   (if (< (+ (sqr x) (sqr y)) 1.0)
;             ;                                                                       0.5
;             ;                                                                       0.0))))))))
;             ;                                      (loop for i below (* nx nx) do
;             ;                                        (setf (aref m i) (* (aref m i) sum)))
;             ;                                      (setf (slot-value (source w) 'current-matrix) m)
;             ;                                      (add-plot (rev-fourier-graph w) (make-instance 'rev-fourier-matrix-plot))))))
;             :append-element
;               (create-element "span"
;                               :|style.display| "inline-block"
;                               :|style.width| "100%"
;                               :|style.marginTop| "1em"
;                               :|style.textAlign| "center"
;                 :append-element
;                   (with-self but
;                     (create-element "button" :|innerHTML| "GO"
;                                              :|style.width| "12em"
;                       :|onclick|
;                         (let ((state nil)
;                               (cur-chi nil)
;                               (plt (make-instance 'rev-fourier-matrix-plot)))
;                           (lambda (ev)
;                             (labels ((mk-step (&optional (n 0))
;                                        (setf *cur-step* n)
;                                        (set-progress calc-bar 0)
;                                        (perform-step (source w)
;                                          (lambda (v)
;                                            (set-progress calc-bar v))
;                                          (lambda ()
;                                            (if (= n 0) (setf cur-chi (current-chi (source w))))
;                                            (redraw plt)
;                                            (redraw (informer w))
;                                            (if state
;                                                (if (> *current-delta-cf* 1.0)
;                                                    (mk-step (+ n 1))
;                                                    (progn
;                                                      (setf state nil)
;                                                      (setf *current-delta-cf* 1000)
;                                                      (setf (jscl::oget but "innerHTML") "GO"))))))))
;                               (setf state (not state))
;                               (if state
;                                   (progn
;                                     (setf (jscl::oget (root calc-bar) "style" "visibility") "visible")
;                                     (redraw (informer w))
;                                     (if (solver-worker (source w)) (kill (solver-worker (source w))))
;                                     (setf (slot-value (source w) 'solver-worker) nil)
;                                     (setf cur-chi 1)
;                                     (let ((m (current-matrix (source w))))
;                                       (loop for i below (sqr (resolution (source w))) do
;                                         (setf (aref m i) 0.0)))
;                                     (loop for p in (profiles (source w)) do (setf (slot-value p 'cur-i) nil))
;                                     (setf (slot-value (source w) 'current-chi) nil)
;                                     (remove-all-plots (rev-fourier-graph w))
;                                     (add-plot (rev-fourier-graph w) plt)
;                                     (setf (jscl::oget but "innerHTML") "STOP")
;                                     (mk-step))
;                                   (progn
;                                     (if (solver-worker (source w)) (kill (solver-worker (source w))))
;                                     (kill (solver-worker (source w)))
;                                     (setf (slot-value (source w) 'solver-worker) nil)
;                                     (setf *current-delta-cf* 1000)
;                                     (setf *cur-step* 0)
;                                     (setf (jscl::oget but "innerHTML") "GO"))))))))))))
;   (root w))
;
; ; (defmethod-f render-widget ((w main-view))
; ;   (setf (slot-value w 'root)
; ;         (create-element "div"
; ;           :append-element
; ;             (create-element "div" :|style.float| "left"
; ;                                   :|style.textAlign| "center"
; ;                                   :|style.width| "12em"
; ;               :append-element
; ;                 (create-element "div" :|style.aspectRatio| "1"
; ;                   :append-element (render-widget (psf-graph w)))
; ;               :append-element
; ;                 (create-element "span"
; ;                                 :|style.display| "inline-block"
; ;                                 :|style.width| "100%"
; ;                                 :|style.marginTop| "1em"
; ;                                 :|style.textAlign| "center"
; ;                   :append-element "FWHM:"
; ;                   :append-element
; ;                     (create-element "span" :|style.width| "1em"
; ;                                            :|style.display| "inline-block")
; ;                   :append-element
; ;                     (render-widget
; ;                       (make-instance 'editable-field
; ;                         :input-size 5
; ;                         :value (format nil "~A" *cur-fwhm*)
; ;                         :ok (lambda (v)
; ;                               (let ((v1 (js-parse-float v)))
; ;                                 (if (> v1 0)
; ;                                     (progn
; ;                                       (setf *cur-fwhm* v1)
; ;                                       (redraw (psf-graph w))
; ;                                       (reblur (picker (sample-src w)))
; ;                                       (remove-all-profiles (sample-src w))
; ;                                       v1))))))
; ;                   :append-element (create-element "span" :|style.width| "1em"
; ;                                                          :|style.display| "inline-block")
; ;                   :append-element "km/s")
; ;               :append-element
; ;                 (create-element "span"
; ;                                 :|style.display| "inline-block"
; ;                                 :|style.width| "100%"
; ;                                 :|style.marginTop| "1em"
; ;                                 :|style.textAlign| "center"
; ;                   :append-element "max V:"
; ;                   :append-element
; ;                     (create-element "span" :|style.width| "1em"
; ;                                            :|style.display| "inline-block")
; ;                   :append-element
; ;                     (render-widget
; ;                       (make-instance 'editable-field
; ;                         :input-size 5
; ;                         :value (format nil "~A" *max-v*)
; ;                         :ok (lambda (v)
; ;                               (let ((v1 (js-parse-float v)))
; ;                                 (if (> v1 0)
; ;                                     (progn
; ;                                       (setf *max-v* v1)
; ;                                       (redraw (trail w))
; ;                                       (redraw (picker (sample-src w)))
; ;                                       (remove-all-profiles (sample-src w))
; ;                                       v1))))))
; ;                   :append-element (create-element "span" :|style.width| "1em"
; ;                                                          :|style.display| "inline-block")
; ;                   :append-element "km/s")
; ;               :append-element
; ;                 (create-element "span"
; ;                                 :|style.display| "inline-block"
; ;                                 :|style.width| "100%"
; ;                                 :|style.marginTop| "1em"
; ;                                 :|style.textAlign| "center"
; ;                   :append-element "resolution:"
; ;                   :append-element
; ;                     (create-element "span" :|style.width| "1em"
; ;                                            :|style.display| "inline-block")
; ;                   :append-element
; ;                     (render-widget
; ;                       (make-instance 'editable-field
; ;                         :input-size 5
; ;                         :value (format nil "~A" *grid-resolution*)
; ;                         :ok (lambda (v)
; ;                               (let ((v1 (js-parse-float v)))
; ;                                 (if (> v1 0)
; ;                                     (progn
; ;                                       (setf *grid-resolution* (floor v1))
; ;                                       (change-resolution (sample-src w))
; ;                                       (remove-all-profiles (sample-src w))
; ;                                       v1))))))
; ;                   :append-element (create-element "span" :|style.width| "1em"
; ;                                                          :|style.display| "inline-block")
; ;                   :append-element "pixels")
; ;               :append-element
; ;                 (create-element "span"
; ;                                 :|style.display| "inline-block"
; ;                                 :|style.width| "100%"
; ;                                 :|style.marginTop| "1em"
; ;                                 :|style.textAlign| "center"
; ;                   :append-element "SNR:"
; ;                   :append-element
; ;                     (create-element "span" :|style.width| "1em"
; ;                                            :|style.display| "inline-block")
; ;                   :append-element
; ;                     (render-widget
; ;                       (make-instance 'editable-field
; ;                         :input-size 5
; ;                         :value (format nil "~A" *cur-snr*)
; ;                         :ok (lambda (v)
; ;                               (let ((v1 (js-parse-float v)))
; ;                                 (if (> v1 0)
; ;                                     (progn
; ;                                       (setf *cur-snr* v1)
; ;                                       (remove-all-profiles (sample-src w))
; ;                                       v1))))))))
; ;           :append-element
; ;             (create-element "div" :|style.float| "left"
; ;                                   :|style.textAlign| "center"
; ;                                   :|style.width| "20em"
; ;               :append-element (render-widget (sample-src w)))
; ;           :append-element
; ;             (create-element "div" :|style.float| "left"
; ;                                   :|style.width| "21em"
; ;                                   :|style.height| "30em"
; ;               :append-element (render-widget (trail w)))
; ;           :append-element
; ;             (create-element "div" :|style.float| "left"
; ;                                   :|style.textAlign| "center"
; ;                                   :|style.width| "20em"
; ;               :append-element (render-widget (fourier-block w))))))
;
; ; (defparameter-f *tst* nil)
; ; (remote-exec
; ;   '(let* ((mv (make-instance 'main-view)))
; ;      (if *tst* (remove-element *tst*))
; ;      (setf *tst* (create-element "div" :|style.overflow| "auto"
; ;                    :append-element (render-widget mv)))
; ;      (append-element *tst*)
; ;      nil))
;
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ; (defmethod-f xmin ((p psf-plot))
; ;   (xmin (parent p)))
; ;
; ; (defmethod-f xmax ((p psf-plot))
; ;   (xmax (parent p)))
; ;
; ; (defmethod-f ymax ((p psf-graph))
; ;   (* (/ 2.0 *cur-fwhm*) (sqrt (/ (jsln 2) pi))))
; ;
; ; (defmethod-f xmin ((g psf-graph))
; ;   (* -2 (fwhm g)))
; ;
; ; (defmethod-f xmax ((g psf-graph))
; ;   (* 2 (fwhm g)))
; ;
; ; (defmethod-f xmin ((g trail-graph))
; ;   (* -1 *max-v*))
; ;
; ; (defmethod-f xmax ((g trail-graph))
; ;   *max-v*)
; ;
; ; (defmethod-f xmin ((g sample-picker))
; ;   (* -1 *max-v*))
; ;
; ; (defmethod-f xmax ((g sample-picker))
; ;   *max-v*)
; ;
; ; (defmethod-f ymin ((g sample-picker))
; ;   (* -1 *max-v*))
; ;
; ; (defmethod-f ymax ((g sample-picker))
; ;   *max-v*)
; ;
; ; (defmethod-f xmin ((g rev-fourier-graph))
; ;   (* -1 *max-v*))
; ;
; ; (defmethod-f xmax ((g rev-fourier-graph))
; ;   *max-v*)
; ;
; ; (defmethod-f ymin ((g rev-fourier-graph))
; ;   (* -1 *max-v*))
; ;
; ; (defmethod-f ymax ((g rev-fourier-graph))
; ;   *max-v*)
; ;
; ; (defmethod-f xmin ((g rev-fourier-matrix-plot))
; ;   (* -1 *max-v*))
; ;
; ; (defmethod-f xmax ((g rev-fourier-matrix-plot))
; ;   *max-v*)
; ;
; ; (defmethod-f ymin ((g rev-fourier-matrix-plot))
; ;   (* -1 *max-v*))
; ;
; ; (defmethod-f ymax ((g rev-fourier-matrix-plot))
; ;   *max-v*)
; ;
; ; (defmethod-f resolution ((g sample-picker))
; ;   *grid-resolution*)
; ;
; ; (defmethod-f resolution ((g profile-source))
; ;   *grid-resolution*)
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
; (defmethod-f change-resolution ((s sample-profile-source))
;   (redraw s))
;
;
;
; (defmethod-f fourier-matrix ((b profile-source))
;   (if (not (slot-value b 'fourier-matrix))
;       (let* ((nx (resolution b))
;              (ny nx))
;         (setf (slot-value b 'fourier-matrix) (make-array `(,nx ,ny)))
;         (let* ((m (slot-value b 'fourier-matrix))
;                (sprofs (sort (mapcar #'cons
;                                      (mapcar #'fourier (profiles b))
;                                      (mapcar #'phase (profiles b)))
;                              #'<
;                              :key #'cdr))
;                (profs (map 'vector #'car sprofs))
;                (phases (map 'vector #'cdr sprofs))
;                (nprofs (length sprofs)))
;           (loop for i below nx do
;             (loop for j below ny do
;               (let* ((x (- (* 2.0 (/ i (- nx 1))) 1.0))
;                      (y (- (* 2.0 (/ j (- ny 1))) 1.0))
;                      (r (sqrt (+ (sqr x) (sqr y))))
;                      (omega (/ (* r pi nx 0.5) *max-v*)))
;                 (setf (aref m (+ i (* nx j)))
;                       (if (or (not sprofs)
;                               (> r 1.0))
;                           '(0 0)
;                           (labels ((bin-search (v ar np key)
;                                      (if (< v (funcall key (aref ar 0)))
;                                          (return-from bin-search (values -1 0 1)))
;                                      (if (> v (funcall key (aref ar (- np 1))))
;                                          (return-from bin-search (values (- np 1) np 0)))
;                                      (do* ((b1 0)
;                                            (b2 (- np 1)))
;                                           ()
;                                        (let ((i (jsfloor (* 0.5 (+ b1 b2)))))
;                                          (if (< v (funcall key (aref ar i)))
;                                              (setf b2 i)
;                                              (setf b1 i))
;                                          (if (= b2 (+ 1 b1))
;                                              (return-from bin-search
;                                                (let ((v1 (funcall key (aref ar b1)))
;                                                      (v2 (funcall key (aref ar b2))))
;                                                  (values b1 b2 (/ (- v v1) (- v2 v1)))))))))
;                                    (search-r (ph)
;                                      (let ((prof (aref profs ph)))
;                                        (multiple-value-bind (i1 i2 cf)
;                                                             (bin-search omega prof
;                                                                         (array-dimension prof 0) #'car)
;                                          (cond ((= cf 0) (cdr (aref prof i1)))
;                                                ((= cf 1) (cdr (aref prof i2)))
;                                                (t (let ((v1 (aref prof i1))
;                                                         (v2 (aref prof i2))
;                                                         (cf1 (- 1.0 cf)))
;                                                     `(,(+ (* cf  (cadr v1))
;                                                           (* cf1 (cadr v2)))
;                                                       ,(+ (* cf  (caddr v1))
;                                                           (* cf1 (caddr v2)))))))))))
;                             (let* ((alpha (jsatan2 y x))
;                                    (phi (+ 0.5 (/ alpha (* pi 2.0)))))
;                               (if (cdr sprofs)
;                                   (multiple-value-bind (ph1 ph2 cf) (bin-search phi phases nprofs #'identity)
;                                     (let* ((ph1 (if (>= ph1 0) ph1 (- nprofs 1)))
;                                            (ph2 (if (< ph2 nprofs) ph2 0))
;                                            (v1 (search-r ph1))
;                                            (v2 (search-r ph2))
;                                            (cf1 (- 1.0 cf)))
;                                       `(,(+ (* cf  (car v1))
;                                             (* cf1 (car v2)))
;                                         ,(+ (* cf  (cadr v1))
;                                             (* cf1 (cadr v2))))))
;                                   (search-r 0))))))))))))
;   (slot-value b 'fourier-matrix))
;
; (defmethod-f current-matrix ((s profile-source))
;   (if (not (slot-value s 'current-matrix))
;       (setf (slot-value s 'current-matrix) (make-array (list (resolution s) (resolution s)) :initial-element 0)))
;       ; (let* ((ms (fourier-matrix s))
;       ;        (fnx (array-dimension ms 0))
;       ;        (fny (array-dimension ms 1))
;       ;        (nx (resolution s))
;       ;        (ny nx)
;       ;        (m (setf (slot-value s 'current-matrix) (make-array `(,nx ,ny))))
;       ;        (minv nil)
;       ;        (maxv nil))
;       ;   (loop for i below nx do
;       ;     (loop for j below ny do
;       ;       (let ((vx (- (* 2.0 (/ i (- nx 1))) 1.0))
;       ;             (vy (- (* 2.0 (/ j (- ny 1))) 1.0))
;       ;             (v 0))
;       ;         (if (< (+ (sqr vx) (sqr vy)) 1.0)
;       ;             (progn
;       ;               (loop for i1 below fnx do
;       ;                 (loop for j1 below fny do
;       ;                   (let ((nfvx (- (* 2.0 (/ i1 (- fnx 1))) 1.0))
;       ;                         (nfvy (- (* 2.0 (/ j1 (- fny 1))) 1.0)))
;       ;                     (if (< (+ (sqr nfvx) (sqr nfvy)) 1.0)
;       ;                         (let ((fv (aref ms (+ i1 (* fnx j1))))
;       ;                               (omegal (* 0.5 pi fny (+ (* vx nfvx) (* vy nfvy)))))
;       ;                           (setf v (+ v
;       ;                                      (* (car fv)  (jscos omegal))
;       ;                                      (* (cadr fv) (jssin omegal)))))))))
;       ;               (let ((v (max 0 v)))
;       ;                 (setf minv (if minv (min minv v) v))
;       ;                 (setf maxv (if maxv (max maxv v) v))
;       ;                 (setf (aref m (+ i (* nx j))) v)))
;       ;             (setf (aref m (+ i (* nx j))) 0)))))
;       ;   (let ((d (/ 1 (- maxv minv))))
;       ;     (loop for i below (* nx ny) do
;       ;       (setf (aref m i) (* d (- (aref m i) minv)))))))
;   (slot-value s 'current-matrix))
;
;
; (defmethod-f matrix ((p fourier-matrix-plot))
;   (if (not (slot-value p 'matrix))
;       (let* ((ms (fourier-matrix (source (parent p)))))
;         (setf (slot-value p 'matrix) (make-array (array-dimensions ms)))
;         (labels ((ampl (x)
;                    (sqrt (+ (sqr (car x)) (sqr (cadr x))))))
;           (let* ((m (slot-value p 'matrix))
;                  (mi (ampl (aref ms 0)))
;                  (ma (ampl (aref ms 0)))
;                  (nx (array-dimension ms 0))
;                  (ny (array-dimension ms 1)))
;             (loop for i below nx do
;               (loop for j below ny do
;                 (let* ((a (ampl (aref ms (+ i (* nx j))))))
;                   (setf mi (min mi a)
;                         ma (max ma a)
;                         (aref m (+ i (* nx (- ny j 1)))) a))))
;             (let ((d (/ 1 (- ma mi))))
;               (loop for i below nx do
;                 (loop for j below ny do
;                   (let ((idx (+ i (* nx j))))
;                     (setf (aref m idx)
;                           (* d (- (aref m idx) mi)))))))))))
;   (slot-value p 'matrix))
;
; (defmethod-f matrix ((p rev-fourier-matrix-plot))
;   (current-matrix (source (parent p))))
;
; (defmethod-f fourier ((p profile))
;   (if (not (fourier-data p))
;       (setf (slot-value p 'fourier-data)
;             (let* ((data (data p))
;                    (np2 (+ 1 (jsfloor (/ (length data) 2))))
;                    (ar (make-array (list np2)))
;                    (cf (/ 1.0 np2)))
;               (loop for i below np2 do
;                 (let ((omega (* pi (/ i *max-v*)))
;                       (sum-cos 0)
;                       (sum-sin 0))
;                   (map nil
;                     (lambda (vi)
;                       (let* ((x (car vi))
;                              (y (cdr vi))
;                              (omegax (* omega x)))
;                         (setf sum-cos (+ sum-cos (* y (jscos omegax))))
;                         (setf sum-sin (+ sum-sin (* y (jssin omegax))))))
;                     data)
;                   (let ((cf (* cf (if (= i 0) 0.5 1.0))))
;                     (setf (aref ar i) (list omega (* cf sum-cos) (* cf sum-sin))))))
;               ar)))
;   (fourier-data p))
;
;
;
;
;
; (defmethod-f mean ((p profile) (s profile-source))
;   (if (not (and (slot-value p 'mean) (equal s (mean-src p))))
;       (setf (slot-value p 'mean)
;             (let* ((dats (mapcar (lambda (p) (map 'vector #'identity (data p))) (profiles s)))
;                    (ld (length dats)))
;               (setf (slot-value p 'mean-src) s)
;               (mapcar (lambda (v)
;                         (/ (loop for d in dats sum
;                              (let ((p1 (position-if (lambda (x) (> (car x) v)) d)))
;                                (if (and p1 (>= p1 0))
;                                    (let* ((l (aref d (- p1 1)))
;                                           (r (aref d p1))
;                                           (lx (car l))
;                                           (rx (car r))
;                                           (ly (cdr l))
;                                           (ry (cdr r)))
;                                      (+ ly (* (- ry ly) (/ (- v lx) (- rx lx)))))
;                                    0)))
;                            ld))
;                       (mapcar #'car (data p))))))
;   (slot-value p 'mean))
;
; (defmethod-f current-chi ((s profile-source))
;   (if (profiles s)
;       (progn
;         (if (not (slot-value s 'current-chi))
;             (let ((m (current-matrix s))
;                   (max-d (max-d s)))
;               (setf (slot-value s 'current-chi) (loop for p in (profiles s) sum (chi p m max-d)))))
;         (slot-value s 'current-chi))
;       0))
;
; (defmethod-f max-d ((s profile-source))
;   (if (not (slot-value s 'max-d))
;       (setf (slot-value s 'max-d)
;             (apply #'max (mapcar #'cdr (apply #'concatenate (cons 'list (mapcar #'data (profiles s))))))))
;   (slot-value s 'max-d))
;
; (defmethod-f solver-worker ((s profile-source))
;   (if (not (slot-value s 'solver-worker))
;       (setf (slot-value s 'solver-worker) (make-instance 'classic-worker :persistent-cache t)))
;   (slot-value s 'solver-worker))
;
; (defmethod-f calc-step ((s profile-source) bar-cb)
;   (let* ((s s)
;          (tim0 ((jscl::oget (jscl::make-new (winref "Date")) "getTime")))
;          (m (current-matrix s))
;          (dbg (funcall bar-cb 0.03))
;          (nx (array-dimension m 0))
;          (ny (array-dimension m 1))
;          (m1 (make-array (list nx ny)))
;          (grad (make-array (list (* nx ny)) :initial-element 0))
;          (delta 1e-3)
;          (profs (profiles s))
;          (dbg (funcall bar-cb 0.05))
;          (prof-dats (mapcar (lambda (p) (mapcar #'cdr (data p))) profs))
;          (prof-cur-i (mapcar (lambda (p) (cur-i p m)) profs))
;          (prof-weights (mapcar #'weights profs))
;          (max-d (max-d s))
;          (chi (current-chi s))
;          (dbg (funcall bar-cb 0.07))
;          (all-data (mapcar (lambda (w d c m)
;                              (list w (- d c) (+ 0.1 (/ m max-d))))
;                            (apply #'concatenate (cons 'list prof-weights))
;                            (apply #'concatenate (cons 'list prof-dats))
;                            (apply #'concatenate (cons 'list prof-cur-i))
;                            (apply #'concatenate
;                                   (cons 'list
;                                         (mapcar (lambda (p)
;                                                   (mean p s))
;                                                 profs)))))
;          (progress-cnt 0)
;          (grad-cnt 0)
;          (adl (length all-data)))
;     (funcall bar-cb 0.1)
;     (loop for ad in all-data sum
;        (let ((w (car ad)))
;          (incf grad-cnt)
;          (if (> (incf progress-cnt) 100)
;              (let ((tim1 ((jscl::oget (jscl::make-new (winref "Date")) "getTime"))))
;                (setf progress-cnt 0)
;                (if (> (- tim1 tim0) 200)
;                    (progn
;                      (setf tim0 tim1)
;                      (funcall bar-cb (+ 0.1 (* 0.8 (/ grad-cnt adl))))))))
;          (loop for i below (length w) by 2 sum
;            (let* ((wl (* (aref w (+ i 1)) delta))
;                   (nv (* (caddr ad) (- (sqr wl) (* 2 wl (cadr ad))))))
;              (setf (aref grad (aref w i))
;                    (+ (aref grad (aref w i)) nv))
;              nv))))
;     (let ((grad-l (/ 1.0 (sqrt (loop for i below (* nx ny) sum (expt (aref grad i) 2)))))
;           (cnt 0))
;       (funcall bar-cb 0.9)
;       (labels ((apply-grad ()
;                  (loop for i below (* nx ny) do
;                    (multiple-value-bind (j1 i1) (floor i nx)
;                      (setf (aref m1 i)
;                            (max 0 (- (aref m i) (* *current-delta-cf* delta (* grad-l (aref grad i))))))))
;                  (setf (slot-value s 'current-matrix) m1)
;                  (loop for p in profs do (setf (slot-value p 'cur-i) nil))
;                  (setf (slot-value s 'current-chi) nil)
;                  (funcall bar-cb 0.95)))
;         (apply-grad)
;         (loop until (or (< (current-chi s) chi) (< *current-delta-cf* 1.0)) do
;           (progn
;             (setf *current-delta-cf* (* 0.5 *current-delta-cf*))
;             (apply-grad)))
;         (if (> *current-delta-cf* 1.0) (setf *current-delta-cf* (* 1.1 *current-delta-cf*)))
;         (values (current-matrix s) (current-chi s) *current-delta-cf*)))))
;
; (defparameter-f *worker-src* nil)
;
; (defmethod-f perform-step ((s profile-source) bar-cb cb)
;   (setf *worker-src* s)
;   (register-main-lambda bar-cb)
;   (funcall bar-cb 0)
;   (bind-exit-values-for (m chi delta)
;     (run-in-web-worker (solver-worker s)
;       (cache-vars t)
;       (calc-step *worker-src* bar-cb))
;     (progn
;       (funcall bar-cb 1)
;       (setf (slot-value s 'current-matrix) m)
;       (setf (slot-value s 'current-chi) chi)
;       (setf *current-delta-cf* delta)
;       (funcall cb))))
;
; (defparameter-f *last-prof* nil)
;
;
; ; (defmethod-f func ((p psf-plot))
; ;   (lambda (x)
; ;     (let ((sigma (/ (fwhm p) (sqrt (* 8.0 (jsln 2.0))))))
; ;       (* (/ 1.0 (* sigma (sqrt (* 2 pi)))) (exp (/ (* -0.5 x x) (* sigma sigma)))))))
;
; ; (defmethod-f fwhm ((g psf-graph))
; ;   *cur-fwhm*)
; ;
; ; (defmethod-f fwhm ((g psf-plot))
; ;   (fwhm (parent g)))
;
; (defmethod-f fwhm ((p sample-picker))
;   *cur-fwhm*)
;
; (defmethod-f remake-profile ((p profile) data)
;   (setf (slot-value p 'data) data)
;   (setf (slot-value p 'fourier-data) nil))
;
; (defmethod-f remake-profile :after ((p sample-profile) data)
;   (setf (slot-value (plot p) 'table) (data p))
;   (redraw (plot p)))
;
; ; (defmethod-f remake-profiles ((s sample-profile-source))
; ;   (labels ((rmp (profs)
; ;              (if profs
; ;                  (let* ((p (car profs)))
; ;                    (remake-profile p (data (get-profile (picker s) (phase p))))
; ;                    (execute-after 0.01 (lambda () (rmp (cdr profs))))))))
; ;     (rmp (profiles s))))
;
;
; (defmethod-f remove-all-plots :after ((g trail-graph))
;   (setf (slot-value g 'last-max) nil))
;
;
;
; (defmethod-f table ((p trail-fplot))
;   (let ((mx (if (last-max (parent p))
;                 (last-max (parent p))
;                 1))
;         (phase (phase p)))
;     (mapcar
;       (lambda (xy)
;         (cons (car xy)
;               (+ phase (* 0.2 (/ (cdr xy) mx)))))
;       (slot-value p 'table))))
;
; (defmethod-f clear ((b fourier-block))
;   (remove-all-plots (fourier-graph b))
;   (remove-all-plots (rev-fourier-graph b)))
;
; ; (defmethod-f jscl::initialize-instance :after ((obj main-view) &rest args)
; ;   (setf (slot-value obj 'sample-src)
; ;         (make-instance 'sample-profile-source :trail (trail obj)))
; ;   (setf (slot-value obj 'fourier-block)
; ;         (make-instance 'fourier-block :source (sample-src obj))))
;
; (defmethod-f jscl::initialize-instance :after ((obj fourier-block) &rest args)
;   (setf (slot-value obj 'fourier-graph) (make-instance 'fourier-graph :source (source obj)))
;   (setf (slot-value obj 'rev-fourier-graph) (make-instance 'rev-fourier-graph :source (source obj))))
;
; (defmethod-f jscl::initialize-instance :after ((obj sample-picker) &rest args)
;   (setf (slot-value obj 'show-scales) '(:left :right :top :bottom))
;   (setf (slot-value obj 'xcaption) (create-element "span" :|style.whiteSpace| "nowrap" :|innerHTML| "Vx (km/s)"))
;   (setf (slot-value obj 'ycaption) (create-element "span" :|style.whiteSpace| "nowrap" :|innerHTML| "Vy (km/s)"))
;   (setf (slot-value obj 'xmin) (* -1 *max-v*))
;   (setf (slot-value obj 'xmax) *max-v*)
;   (setf (slot-value obj 'ymin) (* -1 *max-v*))
;   (setf (slot-value obj 'ymax) *max-v*))
;
; ; (defmethod-f jscl::initialize-instance :after ((obj psf-graph) &rest args)
; ;   (add-plot obj (make-instance 'psf-plot)))
;
; (defmethod-f jscl::initialize-instance :after ((obj profile) &rest args)
;   (let ((data (data obj)))
;     (if (or (< (car (car data)) (- *max-v*))
;             (> (car (car (last data))) *max-v*))
;         (setf (slot-value obj 'data)
;               (remove-if
;                 (lambda (xy)
;                   (or (< (car xy) (- *max-v*))
;                       (> (car xy) *max-v*)))
;                 data)))))
;
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ; (defun-f show-main-page ()
; ;   (append-element (render-widget (make-instance 'main-view))))
;
