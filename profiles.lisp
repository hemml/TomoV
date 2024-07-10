(in-package :tomo)

(defparameter-f *weight-treshhold* 2e-2)

(defclass-f profile (watcher)
  ((data :initarg :data
         :initform (error "Please, provide profile data!")
         :accessor data)
   (weights)
   (cur-i)
   (max-d)
   (prof-mean)
   (offset)
   (phase-weight :initform 1
                 :accessor phase-weight)
   (phase :initarg :phase
          :initform (error "Please, provide profile phase!")
          :accessor phase)
   (plot :initform nil
         :accessor plot)
   (cur-plot :initform nil
             :accessor cur-plot)
   (ads-cache)))


(defclass-f profile-source (watcher)
  ((profiles :accessor profiles
             :initform (list))
   (offset)
   (mean)
   (chi)
   (max-d)
   (trail)))

(defclass-f profile-source-widget (omg-widget profile-source)
  ((src-root :accessor src-root)
   (name :accessor name
         :initform nil)
   (solver :accessor solver)))

(defclass-f save-dialog (modal-dialog-window)
  ((onsave :initarg :onsave
           :accessor onsave)))

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
  (make-instance 'trail-graph :params (params s)))

(defmethod-f reset ((p profile))
  (slot-makunbound p 'cur-i)
  (slot-makunbound p 'prof-mean)
  (slot-makunbound p 'ads-cache))

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
  (apply #'max (mapcar #'cdr (data p))))

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
      (* (phase-weight p) (loop for i in (mapcar #'cdr (data p)) and ci in (cur-i p slv) and pm in (prof-mean p s) sum
                            (* (sqr (- i ci))
                               (if lsnr
                                   (+ nzt (/ (max 0 (- pm ofs)) max-d))
                                   1.0)))))))

(defmethod-f reset ((s profile-source))
  (slot-makunbound s 'chi)
  (map nil #'reset (profiles s)))

(defmethod-f update-profile-plots ((p profile) (s profile-source) &key show-cur-i)
  (when (plot p)
    (remove-plot (plot p)))
  (when (cur-plot p)
    (remove-plot (cur-plot p)))
  (let ((ofs (offset p)))
    (add-plot (trail s) (setf (slot-value p 'plot)
                              (make-instance 'trail-plot
                                :table (mapcar (lambda (xy) (cons (car xy) (max 0 (- (cdr xy) ofs)))) (data p))
                                :phase (phase p))))

    (if show-cur-i
        (add-plot (trail s) (setf (slot-value p 'cur-plot)
                                  (make-instance 'trail-plot
                                    :table (mapcar (lambda (xy) (cons (car xy) (max 0 (- (cdr xy) ofs))))
                                                   (mapcar #'cons
                                                           (mapcar #'car (data p))
                                                           (cur-i p show-cur-i)))
                                    :phase (phase p)
                                    :color "blue"))))))

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
    (let ((inp (create-element "input" :|style.marginLeft| "1em")))
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
  (setf (slot-value s 'root)
        (let* ((slv (solver s)))
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
                                                                                             (indexed-db-add "Tomo" "sources" (name s) buf
                                                                                               :when-ok (lambda ()
                                                                                                          (update (lst *app*) :redraw t)
                                                                                                          (redraw s)
                                                                                                          (if sd (close sd)))
                                                                                               :raw t)))))))
                                                     (if (name s)
                                                         (sav)
                                                         (append-element
                                                           (render-widget
                                                             (with-self sd
                                                               (make-instance 'save-dialog
                                                                 :onsave (lambda (name)
                                                                           (setf (slot-value s 'name) name)
                                                                           (sav sd))))))))))
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
                :append-element (render-widget (trail s)))
            :append-element (render-widget slv)))))
