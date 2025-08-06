(in-package :tomo)

(defparameter-f *weight-treshhold* 2e-2)

(defclass-f profile (watcher)
  ((data :initarg :data
         :initform (error "Please, provide profile data!")
         :accessor data)
   (fourier-data)
   (weights)
   (cur-i)
   (max-d)
   (prof-mean)
   (offset)
   (denoised-data :initform nil
                  :accessor denoised-data)
   (phase-weight :initform 1
                 :accessor phase-weight)
   (phase :initarg :phase
          :initform (error "Please, provide profile phase!")
          :accessor phase)
   (plot :initform nil
         :accessor plot)
   (cur-plot :initform nil
             :accessor cur-plot)
   (den-plot :initform nil
             :accessor den-plot)
   (denoised-plot :initform nil)
   (ads-cache)))


(defclass-conf profile-source (watcher)
  ((profiles :accessor profiles
             :initform (list))
   (offset :desc "Continuum level"
           :type :number
           :initform 1)
   (mean)
   (chi)
   (max-d)
   (trail)
   (solver)))

(defclass-f profile-source-widget (omg-widget profile-source)
  ((src-root :accessor src-root)
   (name :accessor name
         :initform nil)
   (solver :accessor solver)))

(defclass-f save-dialog (modal-dialog-window)
  ((onsave :initarg :onsave
           :accessor onsave)
   (name :initarg :name
         :accessor name
         :initform "new data source")))

(defclass-f delete-dialog (modal-dialog-window)
  ((src :initarg :source
        :accessor src)))

(defmethod-f source-loaded ((s profile-source)))

(lazy-slot offset ((s profile))
  0.0)

(lazy-slot offset ((s profile-source))
  1.0)

(lazy-slot params ((s profile-source))
  (make-instance 'parameters))

(lazy-slot trail ((s profile-source))
  (make-instance 'trail-graph :params (params s) :source s))

(defmethod-f soft-reset ((p profile))
  (slot-makunbound p 'cur-i))

(defmethod-f reset ((p profile))
  (slot-makunbound p 'cur-i)
  (slot-makunbound p 'max-d)
  (slot-makunbound p 'fourier-data)
  (slot-makunbound p 'prof-mean)
  (slot-makunbound p 'ads-cache)
  (slot-makunbound p 'weights)
  (with-slots (cur-plot) p
    (when cur-plot
      (remove-plot cur-plot))
    (setf cur-plot nil)))

; (lazy-slot ads-cache ((p profile) (s art-solver))
;   (let* ((adsp (absorbtion-profile s))
;          (vl (map 'vector #'car adsp))
;          (lst (1- (length adsp)))
;          (av-min (car (aref adsp 0)))
;          (av-max (car (aref adsp lst))))
;     (loop for v in (mapcar #'car (data p)) collect
;       (let* ((i1 (cond ((< v av-min) 0)
;                        ((> v av-max) lst)
;                        (t (max 0 (1- (position-if (lambda (x) (>= x v)) vl))))))
;              (i2 (cond ((< v av-min) 0)
;                        ((> v av-max) lst)
;                        (t (min lst (1+ i1))))))
;         (list i1 i2 (if (= i1 i2) 0 (/ (- v (aref vl i1)) (- (aref vl i2) (aref vl i1)))))))))

(lazy-slot ads-cache ((p profile) (s art-solver))
  (let* ((adsp (absorbtion-profile s))
         (vl (map 'vector #'car adsp))
         (lst (1- (length adsp)))
         (av-min (car (aref adsp 0)))
         (av-max (car (aref adsp lst))))
    (loop for v in (mapcar #'car (data p)) collect
      (cond ((< v av-min) 0)
            ((> v av-max) lst)
            (t (max 0 (1- (position-if (lambda (x) (>= x v)) vl))))))))

(def-local-macro-f cos-int (x f k b)
  `(if (= ,f 0)
       (* ,x (+ (* 0.5 ,k ,x) ,b))
       (/ (+ (* ,k (jscos (* ,f ,x)))
             (* ,f (jssin (* ,f ,x))
                  (+ (* ,k ,x) ,b)))
          (* ,f ,f))))

(def-local-macro-f sin-int (x f k b)
  `(if (= ,f 0)
       0
       (/ (- (* ,k (jssin (* ,f ,x)))
             (* ,f (jscos (* ,f ,x))
                  (+ (* ,k ,x) ,b)))
          (* ,f ,f))))

(lazy-slot fourier-data ((p profile))
  (with-slots (params data phase) p
    (let* ((max-v (max-v params))
           (resolution (resolution params))
           (nsamp (1+ (floor resolution 2)))
           (res (make-array (list nsamp 2) :initial-element 0))
           (vcf (/ pi max-v)))
      (loop for f below nsamp do
        (loop for (d1 d2) on data when d2 do
          (let* ((x1 (* vcf (car d1)))
                 (x2 (* vcf (car d2)))
                 (y1 (cdr d1))
                 (y2 (cdr d2))
                 (k (/ (- y2 y1)
                       (- x2 x1)))
                 (b (- y1 (* k x1))))
              (incf (aref res (* f 2))      (- (cos-int x2 f k b) (cos-int x1 f k b)))
              (incf (aref res (1+ (* f 2))) (- (sin-int x1 f k b) (sin-int x2 f k b))))))
      res)))

(defmethod-f ads-profile ((p profile) (s art-solver))
  (let ((adc (ads-cache p s))
        (adp (absorbtion-profile s)))
    (loop for i in adc collect
      (cons (cadr (aref adp i))
            (caddr (aref adp i))))))

      ; (+ (cdr (aref adp (car i)))
      ;    (* (caddr i)
      ;       (- (cdr (aref adp (cadr i)))
      ;          (cdr (aref adp (car i)))))))))

(lazy-slot max-d ((p profile))
  (let ((max-v (max-v (params p))))
    (apply #'max (mapcar #'cdr (remove-if (lambda (x) (or (< (car x) (- max-v)) (> (car x) max-v)))
                                          (data p))))))

(lazy-slot cur-i ((p profile) (s art-solver))
  (let ((adsp (ads-profile p s))
        (m (matrix s))
        (ofs (+ (offset (source s))
                (offset p))))
    (loop for w across (weights p) and a in adsp collect
      (+ (cdr a)
         (* (car a)
            (+ ofs
               (loop for i below (length w) by 2 sum
                 (* (aref m (aref w i)) (aref w (+ i 1))))))))))

; (lazy-slot prof-mean ((p profile) (s profile-source))
;   (let ((sm (mean s)))
;     (loop for d in (data p) collect
;       (gethash (car d) sm))))

(lazy-slot prof-mean ((p profile) (s profile-source))
  (let* ((mean-deep 2)
         (mean-deep (min mean-deep (floor (1- (length (profiles s))) 2)))
         (profs (sort (copy-seq (profiles s)) #'< :key #'phase))
         (pflg nil))
    (setf (cdr (last profs)) profs) ;; Make a circular list. Dangerous!!!
    (labels ((find-profs (pl &optional prev)
               ; (format t "FP: ~A ~A ~A" (phase p) (phase (car pl)) (length prev))
               (if (and (>= (length prev) mean-deep)
                        (equal (car pl) p))
                   (concatenate 'list
                     (loop for i below mean-deep and x in prev collect x)
                     (loop for i below (1+ mean-deep) and x in pl collect x))
                   (find-profs (cdr pl) (cons (car pl) prev)))))
      (let* ((profs (mapcar #'data (find-profs profs)))
             (lp (/ 1.0 (length profs)))
             (vl (mapcar #'car (data p))))
        (labels ((find-v (dat v)
                   (let ((cvl (car dat))
                         (cdvl (cdr dat)))
                     (if (>= (car cvl) v)
                         (cdr cvl)
                         (if cdvl
                             (if (>= (caar cdvl) v)
                                 (+ (cdar cdvl)
                                    (* (- (cdar cdvl)
                                          (cdr cvl))
                                       (/ (- v (car cvl))
                                          (- (caar cdvl) (car cvl)))))
                                 (find-v cdvl v))
                             (cdr cvl))))))
          (let* ((dat (loop for v in vl collect
                        (* lp (loop for p in profs sum (find-v p v))))))
            dat))))))

(lazy-slot weights ((p profile) &optional prog-cb (prog-min 0) (prog-max 1))
  (let* ((len (length (data p)))
         (nx (resolution (params p)))
         (ny nx)
         (dat (make-array `(,(* nx ny))))
         (fwhm (fwhm (params p)))
         (max-v (max-v (params p)))
         (a (sqr (* 2 (/ (jsln 2) fwhm))))
         (cf (* (/ 2 fwhm) (sqrt (/ (jsln 2) pi))))
         (phi (* (phase p) 2.0 pi))
         (ndx (- (jscos phi)))
         (ndy (- (jssin phi)))
         (res-ar (make-array (list len)))
         (pcnt 0)
         (tim0 ((jscl::oget (jscl::make-new (winref "Date")) "getTime"))))
    (if prog-cb (funcall prog-cb prog-min))
    (loop for n below len and v in (mapcar #'car (data p))
      when (and prog-cb (> (incf pcnt) 10)) do
          (let ((tim1 ((jscl::oget (jscl::make-new (winref "Date")) "getTime"))))
            (setf pcnt 0)
            (when (> (- tim1 tim0) 200)
              (setf tim0 tim1)
              (funcall prog-cb (+ prog-min (* (- prog-max prog-min) (/ n len))))))
      do (setf (aref res-ar n)
               (let* ((ar (loop for idx below (* nx ny) and
                            r = (multiple-value-bind (j i) (floor idx nx)
                                  (let* ((y (- (* 2.0 (/ j (- ny 1))) 1.0))
                                         (x (- (* 2.0 (/ i (- nx 1))) 1.0)))
                                    (if (< (+ (sqr x) (sqr y)) 1.0)
                                        (let ((r (exp (* -1 a (sqr (- v (* max-v (+ (* ndy y) (* ndx x)))))))))
                                          (if (> r *weight-treshhold*)
                                              (cons idx (* cf r)))))))
                            when r collect r))
                      (res (make-array (list (* 2 (length ar))))))
                 (loop for i below (length ar) and v in ar do
                   (setf (aref res (* i 2)) (car v))
                   (setf (aref res (+ 1 (* i 2))) (cdr v)))
                 res)))
    res-ar))

(lazy-slot max-d ((s profile-source))
  (apply #'max (mapcar #'max-d (profiles s))))

(lazy-slot mean ((s profile-source))
  (let* ((profs (mapcar #'data (profiles s)))
         (all-v (remove-duplicates (apply #'concatenate (cons 'list (mapcar (lambda (p) (mapcar #'car p)) profs)))))
         (res (make-hash-table))
         (lp (/ 1.0 (length profs))))
    (labels ((find-v (dat v)
               (let ((cvl (car dat))
                     (cdvl (cdr dat)))
                 (if (>= (car cvl) v)
                     (cdr cvl)
                     (if cdvl
                         (if (>= (caar cdvl) v)
                             (+ (cdar cdvl)
                                (* (- (cdar cdvl)
                                      (cdr cvl))
                                   (/ (- v (car cvl))
                                      (- (caar cdvl) (car cvl)))))
                             (find-v cdvl v))
                         (cdr cvl))))))
      (loop for v in all-v do
        (setf (gethash v res) (* lp (loop for p in profs sum (find-v p v)))))
      res)))

; (lazy-slot chi ((s profile-source) m)
;   (let ((max-d (max-d s)))
;     (loop for p in (profiles s) sum
;       (loop for i in (mapcar #'cdr (data p)) and ci in (cur-i p m) and m in (prof-mean p s) sum
;          (* (+ *noize-treshhold* (/ m max-d)) (sqr (- i ci)))))))

(lazy-slot chi ((s profile-source) (slv art-solver))
  (let ((max-d (max-d s))
        (lsnr (low-snr slv))
        (nzt (noize-treshold slv))
        (ofs (offset s)))
    (loop for p in (profiles s) sum
      (with-slots (data denoised-data) p
        (* (phase-weight p) (loop for i in (mapcar #'cdr (if denoised-data denoised-data data)) and ci in (cur-i p slv) and pm in (prof-mean p s) sum
                              (* (sqr (- i ci))
                                 (if lsnr
                                     (+ nzt (/ (max 0 (- pm ofs)) max-d))
                                     1.0))))))))

(defmethod-f denoise ((s profile-source) level)
  (with-slots (profiles params) s
    (let* ((resolution (resolution params))
           (max-v (max-v params))
           (nsamp (1+ (floor resolution 2)))
           (vcf (/ pi max-v)))
      (map nil (lambda (p)
                 (with-slots (phase data denoised-data) p
                   (labels ((get-mid-p (p1p2 mid-ph &optional rev)
                              (when p1p2
                                (destructuring-bind ((ph1 p1) (ph2 p2)) p1p2
                                  (let ((dph (- ph2 ph1)))
                                    (when (< dph 0.25)
                                      (let ((fprof (make-array (list nsamp 2)))
                                            (cf1 (/ (- mid-ph ph1) dph))
                                            (cf2 (/ (- ph2 mid-ph) dph))
                                            (f1 (fourier-data p1))
                                            (f2 (fourier-data p2)))
                                        (loop for i below nsamp do
                                          (setf (aref fprof (* i 2))      (+ (* cf1 (aref f1 (* i 2)))
                                                                             (* cf2 (aref f2 (* i 2))))
                                                (aref fprof (1+ (* i 2))) (+ (* cf1 (aref f1 (1+ (* i 2))))
                                                                             (* cf2 (aref f2 (1+ (* i 2)))))))
                                        (loop for x in data collect
                                          (* (/ 1 pi)
                                             (loop for i below nsamp sum
                                               (if (= i 0)
                                                   (* 0.5 (aref fprof 0))
                                                   (+ (* (aref fprof (* i 2))
                                                         (jscos (* (car x) vcf i)))
                                                      (* (aref fprof (1+ (* i 2)))
                                                         (jssin (* (car x) vcf i))
                                                         (if rev -1 1))))))))))))))
                     (let* ((rev-ph (+ phase 0.5))
                            (profs (sort (copy-seq profiles) #'< :key #'phase))
                            (profs (cons (cons (1- (phase (car (last profs)))) (last profs))
                                         (append (mapcar #'list (mapcar #'phase profs) profs)
                                                 (mapcar #'list (mapcar #'1+ (mapcar #'phase profs)) profs))))
                            (rev-p (get-mid-p (loop for (p1 p2) on profs when (and p1 p2 (<= (car p1) rev-ph) (>= (car p2) rev-ph))
                                                 return (list p1 p2))
                                              rev-ph))
                            (sam-p (get-mid-p (when (> level 1)
                                                (loop for (p1 p2 p3) on profs when (and p1 p2 p3 (eql (cadr p2) p)) do
                                                    (jslog (car p1) (car p2) (car p3))
                                                    (return (list p1 p3))))
                                              phase
                                              t))
                            (cf (/ 1 (+ 1 (if rev-p 1 0) (if sam-p 1 0))))
                            (zdata (loop for p in data collect nil)))
                          (setf denoised-data
                                (mapcar (lambda (d r s)
                                          (let ((dv (cdr d)))
                                            (labels ((irdv (r)
                                                       (if r
                                                           (if (< (abs (/ (- r dv) dv)) 0.5)
                                                               r
                                                               dv)
                                                           0)))
                                              (cons (car d) (* cf (+ dv (irdv r) (irdv s)))))))
                                        data
                                        (if rev-p rev-p zdata)
                                        (if sam-p sam-p zdata)))))))
               profiles))
    (map nil (lambda (p) (update-profile-plots p s)) profiles)
    (redraw (trail s))))

(defmethod-f reset ((s profile-source))
  (slot-makunbound s 'chi)
  (map nil #'reset (profiles s)))

(defmethod-f soft-reset ((s profile-source))
  (slot-makunbound s 'chi)
  (map nil #'soft-reset (profiles s)))


(defmethod-f update-profile-plots ((p profile) (s profile-source) &key show-cur-i)
  (let ((offset (offset p)))
    (with-slots (plot cur-plot den-plot den-plot data denoised-data phase) p
      (if (plot p)
          (with-slots (table) plot
            (setf table data))
          (add-plot (trail s) (setf plot
                                    (make-instance 'trail-plot
                                      :table (mapcar (lambda (xy)
                                                       (cons (car xy) (max 0 (- (cdr xy) offset)))) data)
                                      :phase phase))))
      (if cur-plot
          (if (slot-boundp s 'solver)
            (with-slots (table) cur-plot
              (with-slots (solver) s
                (setf table (mapcar #'cons
                                    (mapcar #'car data)
                                    (cur-i p (solver solver)))))))
          (when show-cur-i
                (add-plot (trail s) (setf cur-plot
                                          (make-instance 'trail-plot
                                            :table (mapcar (lambda (xy) (cons (car xy) (max 0 (- (cdr xy) offset))))
                                                           (mapcar #'cons
                                                                   (mapcar #'car data)
                                                                   (cur-i p show-cur-i)))
                                            :phase phase
                                            :color "blue")))))
      (when denoised-data
        (let ((tbl (mapcar (lambda (xy) (cons (car xy) (max 0 (- (cdr xy) offset)))) denoised-data)))
          (if den-plot
              (with-slots (table) den-plot
                (setf table tbl))
              (add-plot (trail s) (setf den-plot
                                        (make-instance 'trail-plot
                                          :table tbl
                                          :phase phase
                                          :color "green")))))))))


(defmethod-f add-profile ((s profile-source) (p profile))
  (setf (slot-value s 'profiles) (sort (cons p (profiles s)) #'< :key #'phase))
  (update-profile-plots p s))

(defmethod-f remove-all-profiles ((s profile-source))
  (setf (slot-value s 'profiles) (list))
  (remove-all-plots (trail s))
  (reset s))

(defmethod-f remove-profile ((s profile-source) (p profile))
  (setf (slot-value s 'profiles) (remove-if (lambda (x) (eql x p)) (profiles s)))
  (if (and (slot-boundp p 'plot) (plot p)) (remove-plot (plot p)))
  (if (and (slot-boundp p 'cur-plot) (cur-plot p)) (remove-plot (cur-plot p)))
  (reset s))

(defmethod-f get-controls ((s profile-source) node &optional img matrix)
  nil)

(lazy-slot solver ((s profile-source-widget))
  (make-instance 'solver-widget :params (params s) :source s))

(defmethod-f render-widget :after ((sd save-dialog))
  (append-element
    (let ((inp (create-element "input" :|style.marginLeft| "1em"
                                       :value (name sd))))
      (create-element "div" :|style.border| "0.1em solid black"
                            :|style.border-radius| "0.5em"
                            :|style.padding| "0.5em"
                            :|style.background| "#fffff0"
        :append-element (create-element "div" :|style.width| "100%"
                                              :|style.paddingBottom| "1em"
                          :append-element "Save data as:"
                          :append-element inp)
        :append-element (create-element "div" :|style.width| "100%"
                          :append-element
                            (create-element "button" :|innerHTML| "Save"
                                                     :|onclick| (lambda (ev)
                                                                  (funcall (onsave sd) (jscl::oget inp "value"))))
                          :append-element
                            (create-element "button" :|innerHTML| "Cancel"
                                                     :|style.marginLeft| "1em"
                                                     :|onclick| (lambda (ev)
                                                                  (close sd))))))
    (root sd)))

(defmethod-f render-widget :after ((dd delete-dialog))
  (append-element
    (let ((inp (create-element "input" :|style.marginLeft| "1em")))
      (create-element "div" :|style.border| "0.1em solid black"
                            :|style.border-radius| "0.5em"
                            :|style.padding| "0.5em"
                            :|style.background| "#fffff0"
        :append-element (create-element "div" :|style.width| "100%"
                                              :|style.paddingBottom| "1em"
                          :append-element "Delete the image?")
        :append-element (create-element "div" :|style.width| "100%"
                          :append-element
                            (create-element "button" :|innerHTML| "Delete"
                                                     :|onclick| (lambda (ev)
                                                                  (close dd)
                                                                  (if (name (src dd))
                                                                      (indexed-db-delete ("Tomo" "sources" (name (src dd)))
                                                                        (remove-element (root (src dd)))
                                                                        (update (lst *app*) :redraw t))
                                                                      (progn
                                                                        (remove-element (root (src dd)))
                                                                        (update (lst *app*) :redraw t)))))
                          :append-element
                            (create-element "button" :|innerHTML| "Cancel"
                                                     :|style.marginLeft| "1em"
                                                     :|onclick| (lambda (ev)
                                                                  (close dd))))))
    (root dd)))

(defmethod-f render-widget ((s profile-source-widget))
  (when (slot-boundp s 'trail)
    (with-slots (trail) s
      (when (not (slot-boundp trail 'source)) ;; Backward compatibility
        (setf (slot-value trail 'source) s)
        (setf (slot-value trail 'curv-cf) 0.1))))
  (setf (slot-value s 'root)
        (let* ((slv (solver s)))
          (setf (slot-value s 'solver) slv)
          (create-element "div" :|style.float| "left"
                                :|style.textAlign| "center"
                                :|style.border| "1px solid black"
                                :|style.paddingBottom| "1em"
                                :|style.borderRadius| "0.25em"
                                :|style.marginTop| "0.5em"
                                :|style.background| "white"
            :append-element (create-element "div" :|style.width| "100%"
                                                  :|style.marginBottom| "1em"
                                                  :|style.textAlign| "center"
                                                  :|style.background| "rgb(255,184,132)"
                                                  :|style.borderBottom| "1px solid black"
                                                  :|style.paddingTop| "0.2em"
                                                  :|style.paddingBottom| "0.2em"
                                                  :|style.display| "table"
                              :append-element
                                (create-element "button" :|style.display| "table-cell"
                                                         :|style.marginLeft| "1em"
                                                         :|style.width| "10em"
                                                         :|innerHTML| "setings"
                                  :|onclick| (lambda (ev) (append-element (render-widget (params s)))))
                              :append-element
                                (create-element "span" :|style.width| "100%"
                                                       :|style.display| "table-cell"
                                  :append-element
                                    (create-element "button" :|style.display| "table-cell"
                                                             :|style.marginRight| "1em"
                                                             :|innerHTML| "export"
                                      :|onclick| (lambda (ev)
                                                   (append-element
                                                     (render-widget (make-instance 'load-store-progress
                                                                      :obj s :label "Storing data:" :direction :out
                                                                      :final-cb (lambda (buf siz)
                                                                                  (let* ((blob (jscl::make-new  (winref "Blob")
                                                                                                 (jscl::make-new (winref "Array") buf)
                                                                                                 (make-js-object :|type| "application/octet-stream")))
                                                                                         (url ((jscl::oget (jscl::lisp-to-js (jscl::%js-vref "URL")) "createObjectURL") blob))
                                                                                         (el (create-element "a" :|href| url
                                                                                                                 :|download| (format nil "~A.tmv"
                                                                                                                               (if (name s)
                                                                                                                                   (name s)
                                                                                                                                   "unnamed")))))
                                                                                    (append-element el)
                                                                                    ((jscl::oget el "click"))
                                                                                    (remove-element el)
                                                                                    ((jscl::oget (jscl::lisp-to-js (jscl::%js-vref "URL")) "revokeObjectURL") url))))))))
                                  :append-element (if (name s) (name s) "New data source")
                                  :append-element
                                    (create-element "button" :|style.display| "table-cell"
                                                             :|style.marginLeft| "1em"
                                                             :|innerHTML| "save"
                                      :|onclick| (lambda (ev)
                                                   (labels ((sav (&optional sd)
                                                              (append-element
                                                                (render-widget (make-instance 'load-store-progress
                                                                                 :obj s :label "Saving data:" :direction :out
                                                                                 :final-cb (lambda (buf siz)
                                                                                             (indexed-db-get-all-keys (val ("Tomo" "sources"))
                                                                                               (if (position (name s) val :test #'equal)
                                                                                                   (indexed-db-put "Tomo" "sources" (name s) buf
                                                                                                     :when-ok (lambda ()
                                                                                                                (update (lst *app*) :redraw t)
                                                                                                                (redraw s)
                                                                                                                (if sd (close sd)))
                                                                                                     :raw t)
                                                                                                   (indexed-db-add "Tomo" "sources" (name s) buf
                                                                                                     :when-ok (lambda ()
                                                                                                                (update (lst *app*) :redraw t)
                                                                                                                (redraw s)
                                                                                                                (if sd (close sd)))
                                                                                                     :raw t)))))))))
                                                     (append-element
                                                       (render-widget
                                                         (with-self sd
                                                           (make-instance 'save-dialog
                                                             :onsave (lambda (name)
                                                                       (setf (slot-value s 'name) name)
                                                                       (sav sd))
                                                             :name (if (name s)
                                                                       (name s)
                                                                       "new source"))))))))
                                  :append-element
                                    (create-element "button" :|style.display| "table-cell"
                                                             :|style.marginLeft| "0.5em"
                                                             :|innerHTML| "delete"
                                      :|onclick| (lambda (ev)
                                                   (append-element (render-widget (make-instance 'delete-dialog :source s))))))

                              :append-element
                                (create-element "button" :|style.display| "table-cell"
                                                         :|style.marginRight| "1em"
                                                         :|style.width| "10em"
                                                         :|innerHTML| "close"
                                  :|onclick| (lambda (ev)
                                               (reset (solver slv))
                                               (remove-element (root s)))))
            :append-element (setf (slot-value s 'src-root) (create-element "div" :|style.float| "left"
                                                                                 :|style.textAlign| "center"))
            :append-element
              (create-element "div" :|style.width| "20em"
                                    :|style.height| "30em"
                                    :|style.float| "left"
                :append-element (render-widget (trail s))
                :append-element (create-element "span"
                                  :append-elements (render-config (trail s))
                                  :append-element (create-element "button" :|innerHTML| "autoscale"
                                                                           :|style.marginTop| "1em"
                                                    :|onclick| (lambda (ev)
                                                                 (with-slots (curv-cf) (trail s)
                                                                   (setf curv-cf nil)
                                                                   (curv-cf (trail s))
                                                                   (redraw (trail s))
                                                                   (redraw-config (trail s)))))
                                  :append-element
                                    (let* ((denoise-level 1)
                                           (lev (make-instance 'editable-field :value denoise-level
                                                  :ok (lambda (val)
                                                        (let ((v (ignore-errors (parse-integer val))))
                                                          (when v
                                                            (setf denoise-level v)))))))
                                      (create-element "div" :|style.marginTop| "1em"
                                        :append-element (create-element "span" :|innerHTML| "Denoise level:"
                                                                               :|style.marginRight| "1em")
                                        :append-element (render-widget lev)
                                        :append-element (create-element "button" :|innerHTML| "denoise"
                                                                                 :|style.marginLeft| "1em"
                                                          :|onclick| (lambda (ev)
                                                                       (denoise s denoise-level)))))))
            :append-element (render-widget slv)))))
