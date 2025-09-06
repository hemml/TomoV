(in-package :tomo)

(defclass-conf art-solver (watcher)
  ((source :initarg :source
           :accessor source)
   (matrix :accessor matrix)
   (cur-step :initform nil
             :accessor cur-step)
   (delta :initform 1.0e4
          :accessor delta)
   (ads-delta)
   (emm-delta)
   (wrk :accessor wrk)
   (solver-chi)
   (noize-treshold :initform 0.97
                   :desc "Noize theshold"
                   :type :number)
   (low-snr :initform nil
            :accessor low-snr
            :desc "Low SNR mode"
            :type :checkbox)
   (fit-abs :initform nil
            :desc "Fit absorbtion"
            :type :checkbox)
   (fit-emm :initform nil)
            ; :desc "Fit extra emission"
            ; :type :checkbox)
   (gauss-abs-cfs) ;; (center amplitude fwhm)
   (gauss-abs :initform nil
              :desc "Gaussian absorbtion"
              :type :checkbox)
   (absorbtion-profile)
   (iteration-limit :initform 0
                    :desc "Iteration limit"
                    :type :number)))

(defclass-f solver-widget (omg-widget watcher)
  ((solver :accessor solver)))

(defclass-f zoomed-images (modal-dialog-window)
  ((img :initarg :img
        :accessor img)
   (ctrls :initarg :ctrls
          :accessor ctrls)))

(lazy-slot iteration-limit ((s art-solver))
  0)

(lazy-slot fit-abs ((s art-solver))
  nil)

(lazy-slot fit-emm ((s art-solver))
  nil)

(lazy-slot ads-delta ((s art-solver))
  1.0e-3)

(lazy-slot emm-delta ((s art-solver))
  1.0e-3)

(lazy-slot absorbtion-profile ((s art-solver))
  (let* ((len (resolution (params s)))
         (arr (make-array (list len)))
         (max-v (max-v (params s))))
    (loop for i below len do
      (setf (aref arr i) (list (* 2 max-v (- (/ i (1- len)) 0.5))
                               1.0
                               0.0)))
    arr))

(lazy-slot gauss-abs-cfs ((s art-solver))
  (list 0 0 1.0)) ;;(max-v (params s)))

(defparameter-f *current-solver* nil)

(lazy-slot noize-treshold ((s art-solver))
  0.97)

(lazy-slot solver-chi ((s art-solver))
  (chi (source s) s))

(defmethod-f reset ((s art-solver))
  (if (slot-boundp s 'wrk) (kill (wrk s)))
  (setf (slot-value s 'wrk) (make-instance 'classic-worker :persistent-cache t))
  (setf (slot-value (source s) 'chi) nil)
  (setf (slot-value s 'delta) 1.0e4)
  (setf (slot-value s 'ads-delta) 1.0e-3)
  (setf (slot-value s 'emm-delta) 1.0e-3)
  (setf (slot-value s 'cur-step) nil)
  (let ((m (matrix s)))
    (loop for i below (apply #'* (array-dimensions m)) do
      (setf (aref m i) 0))) ;;(offset (source s)))))
  (slot-makunbound s 'absorbtion-profile)
  (slot-makunbound s 'gauss-abs-cfs)
  (reset (source s)))

(lazy-slot matrix ((s art-solver))
  (make-array (list (resolution (params s)) (resolution (params s))) :initial-element 0))

(defmethod-f calc-step ((s art-solver) bar-cb)
  (labels ((get-data (p)
             (with-slots (data denoised-data) p
               (if denoised-data
                   denoised-data
                   data))))
    (let* ((tim0 ((jscl::oget (jscl::make-new (winref "Date")) "getTime")))
           (m (matrix s))
           (dbg (funcall bar-cb 0.03))
           (nx (array-dimension m 0))
           (ny (array-dimension m 1))
           (m1 (make-array (list nx ny)))
           (profs (profiles (source s)))
           (grad)
           (grad-copy)
           (delta (delta s))
           (step 1e-3)
           (dbg (funcall bar-cb 0.05))
           (prof-dats (mapcar (lambda (p) (mapcar #'cdr (get-data p))) profs))
           (prof-vs   (mapcar (lambda (p) (mapcar #'car (get-data p))) profs))
           (prof-cur-i (mapcar (lambda (p) (cur-i p s)) profs))
           (prof-weights (mapcar #'weights profs))
           (chi (chi (source s) s))
           (max-d (max-d (source s)))
           (dbg (funcall bar-cb 0.07))
           (lsnr (low-snr s))
           (nzt (noize-treshold s))
           (ofs (offset (source s)))
           (all-data (mapcar (lambda (w d c pw ads npr)
                               (list w
                                     (- d c)
                                     pw
                                     ads
                                     npr))
                             (apply #'concatenate (cons 'list prof-weights))
                             (apply #'concatenate (cons 'list prof-dats))
                             (apply #'concatenate (cons 'list prof-cur-i))
                             (loop for p in profs append
                               (mapcar (constantly (phase-weight p)) (get-data p)))
                             (loop for p in profs append
                               (ads-profile p s))
                             (loop for p in profs and i below (length profs) append
                               (mapcar (constantly i) (get-data p)))))
           (progress-cnt 0)
           (grad-cnt 0)
           (adl (length all-data))
           (nprofs (length profs)))
      (progn
        (funcall bar-cb 0.1)
        (setf grad (make-array (list (* nx ny)) :initial-element 0)
              grad-cnt 0)
        (loop for ad in all-data sum
          (destructuring-bind (w dc pw ads npr) ad
            (incf grad-cnt)
            (if (> (incf progress-cnt) 100)
                (let ((tim1 ((jscl::oget (jscl::make-new (winref "Date")) "getTime"))))
                  (setf progress-cnt 0)
                  (if (> (- tim1 tim0) 200)
                      (progn
                        (setf tim0 tim1)
                        (funcall bar-cb (+ 0.1 (* 0.7 (/ grad-cnt adl))))))))
            (loop for i below (length w) by 2 sum
              (let* ((wl (* (car ads) (aref w (+ i 1)) step))
                     (nv (* pw (- (sqr wl) (* 2 wl dc))))
                     (idx (aref w i)))
                (setf (aref grad idx)
                      (+ (aref grad idx) nv))
                nv)))))
      (let ((grad-l (/ 1.0 (sqrt (loop for i below (* nx ny) sum (expt (aref grad i) 2)))))
            (cnt 0))
        (funcall bar-cb 0.75)
        (labels ((apply-grad ()
                   (loop for i below (* nx ny) do
                     (setf (aref m1 i)
                           (max 0 (- (aref m i) (* step delta (* grad-l (aref grad i)))))))
                   (soft-reset (source s))
                   (setf (slot-value s 'matrix) m1)))
          (apply-grad)
          (loop until (or (< (chi (source s) s) chi) (< delta 1.0)) do
            (progn
              (setf (slot-value s 'matrix) m)
              (setf delta (* 0.5 delta))
              (apply-grad)))
          (when lsnr
            (with-slots (matrix) s
              (let ((max (* nzt (loop for v across matrix maximize v)))
                    (cnt 0))
                (loop for i below (* nx ny) when (>= (aref matrix i) max) do
                  (setf (aref matrix i) (* nzt max))
                  (incf cnt))
                (jslog "CUT:" cnt))))
          (when (> (chi (source s) s) chi)
            (setf (slot-value s 'matrix) m)
            (soft-reset (source s)))

          (funcall bar-cb 0.8)
          (when (or (fit-abs s) (fit-emm s))
            (with-slots (gauss-abs) s
              (let* ((adsp (absorbtion-profile s))
                     (gcfs (gauss-abs-cfs s))
                     (ldsp (length adsp))
                     (grad (make-array (list ldsp) :initial-element 0))
                     (grad-i (make-array (list ldsp) :initial-element 0))
                     (grad-g (make-array '(3) :initial-element 0))
                     (prof-cur-i (apply #'concatenate (cons 'list (mapcar (lambda (p) (cur-i p s)) profs))))
                     (prof-vals (apply #'concatenate (cons 'list prof-dats)))
                     (prof-v (apply #'concatenate (cons 'list prof-vs)))
                     (prof-adsc (apply #'concatenate (cons 'list (mapcar (lambda (p) (ads-cache p s)) profs))))
                     (ph-weights (loop for p in profs append (mapcar (constantly (phase-weight p)) (get-data p))))
                     (f-a (fit-abs s))
                     (f-e (fit-emm s))
                     (maxv1 (/ 1 (max-v (params s)))))
                (destructuring-bind (amp mid gdisp) gcfs
                  (labels ((mkgs (x s1 s2 s3)
                             (- 1.0
                                (max 0.0
                                     (min 1.0
                                          (* (+ amp s1)
                                             (exp (* -1 (+ gdisp s2) (sqr (- (* maxv1 x) (+ mid s3)))))))))))
                    (loop for ci in prof-cur-i and cp in prof-vals and cc in prof-adsc and phw in ph-weights and v in prof-v do
                      (destructuring-bind (vv ads emm) (aref adsp cc)
                        (when f-a
                          (if gauss-abs
                              (setf (aref grad-g 0) (+ (aref grad-g 0)
                                                       (* phw
                                                          (- (sqr (- cp (+ emm (* (- ci emm) (/ (mkgs v step 0 0) ads)))))
                                                             (sqr (- cp ci)))))
                                    (aref grad-g 1) (+ (aref grad-g 1)
                                                       (* phw
                                                          (- (sqr (- cp (+ emm (* (- ci emm) (/ (mkgs v 0 step 0) ads)))))
                                                             (sqr (- cp ci)))))
                                    (aref grad-g 2) (+ (aref grad-g 2)
                                                       (* phw
                                                          (- (sqr (- cp (+ emm (* (- ci emm) (/ (mkgs v 0 0 step) ads)))))
                                                             (sqr (- cp ci))))))
                              (setf (aref grad cc)
                                    (+ (aref grad cc)
                                       (* phw
                                          (- (sqr (- cp (+ emm (* (- ci emm) (/ (+ ads step) ads)))))
                                             (sqr (- cp ci))))))))
                        (when f-e
                            (setf (aref grad-i cc)
                                  (+ (aref grad-i cc)
                                     (* phw
                                        (- (sqr (- cp (+ ci step)))
                                           (sqr (- cp ci)))))))))
                    (funcall bar-cb 0.9)
                    (macrolet ((mk-ae-step (grad fn del dmin dmax ddef gauss)
                                `(let ((grad-l (sqrt (loop for x across ,grad sum (sqr x)))))
                                   (when (> grad-l 0)
                                     (let* ((grad-l (/ 1.0 grad-l))
                                            (adelta (max 1.e-5 (,del s)))
                                            (c0 chi)
                                            (old-adsp (map 'vector #',fn adsp))
                                            (safe-adsp (copy-seq old-adsp))
                                            (smooth-cf 0.33)
                                            (smooth-len 2))
                                       (labels ((apply-grad ()
                                                  (loop for i below ldsp do
                                                    (setf (,fn (aref adsp i))
                                                          (if ,gauss
                                                              (mkgs (car (aref adsp i))
                                                                    (* -1 adelta step grad-l (aref ,grad 0))
                                                                    (* -1 adelta step grad-l (aref ,grad 1))
                                                                    (* -1 adelta step grad-l (aref ,grad 2)))
                                                              (max ,dmin (min ,dmax (- (,fn (aref adsp i))
                                                                                       (* adelta step grad-l (aref ,grad i))))))))
                                                  ; (format t "ADSP: ~A" adsp)
                                                  (soft-reset (source s))))
                                         (apply-grad)
                                         ; (format t "GRAD: ~A" ,grad)
                                         ; (format t "ADS: ~A" adsp)
                                         (loop while (and (> (chi (source s) s) c0) (> adelta 1e-5)) do
                                           (progn
                                             (setf adelta (* adelta 0.5))
                                             (jslog "DELTA:" adelta)
                                             (loop for i below ldsp do (setf (,fn (aref adsp i)) (aref old-adsp i)))
                                             (apply-grad)
                                             (soft-reset (source s))))
                                         (if (< (chi (source s) s) c0)
                                             (progn
                                               (loop while (< (- (chi (source s) s) c0) -1e-8) do
                                                 (progn
                                                   (jslog "CHI:" (chi (source s) s) c0 adelta (- (chi (source s) s) c0))
                                                   (setf adelta (* adelta 1.5))
                                                   (setf c0 (chi (source s) s))
                                                   (setf old-adsp (map 'vector #',fn adsp))
                                                   (apply-grad)))
                                               (when (< delta 1.0)
                                                 (setf delta 1.1)))
                                             (jslog "CHI1:" (chi (source s) s) c0 adelta (- (chi (source s) s) c0)))
                                         ; (loop for i below ldsp do
                                         ;   (setf (,fn (aref adsp i))
                                         ;         (+ (* smooth-cf (if (> i 0) (aref old-adsp (1- i)) ,ddef))
                                         ;            (* (- 1 (* 2 smooth-cf)) (aref old-adsp i))
                                         ;            (* smooth-cf (if (< i (1- ldsp)) (aref old-adsp (1+ i)) ,ddef)))))
                                         ; (format t "GCFS1: ~A" (gauss-abs-cfs s))
                                         (if ,gauss
                                             (progn
                                               (when (> adelta 1e-5)
                                                 (jslog "SET!!!")
                                                 (setf (car gcfs)   (+ (car gcfs)   (* -1 adelta step grad-l (aref ,grad 0)))
                                                       (cadr gcfs)  (+ (cadr gcfs)  (* -1 adelta step grad-l (aref ,grad 1)))
                                                       (caddr gcfs) (+ (caddr gcfs) (* -1 adelta step grad-l (aref ,grad 2)))))
                                               (loop for i below ldsp do
                                                 (setf (,fn (aref adsp i))
                                                       (aref old-adsp i))))
                                             (loop for i below ldsp do
                                               (setf (,fn (aref adsp i))
                                                     (/ (loop for j from (- i smooth-len) to (+ i smooth-len) sum
                                                          (if (and (>= j 0)
                                                                   (< j ldsp))
                                                              (aref old-adsp j)
                                                              ,ddef))
                                                        (1+ (* 2 smooth-len))))))
                                         ; (format t "GCFS2: ~A" (gauss-abs-cfs s))
                                         (soft-reset (source s))
                                         (when (> (chi (source s) s) c0)
                                           (loop for i below ldsp do (setf (,fn (aref adsp i)) (aref safe-adsp i)))
                                           (soft-reset (source s)))
                                         (setf (slot-value s ',del) adelta)))))))
                      ;;(format t "GG: ~A ~A" gcfs grad-g)
                      (if f-a
                          (if gauss-abs
                              (mk-ae-step grad-g cadr ads-delta 1e-3 1.0 1.0 t)
                              (mk-ae-step grad cadr ads-delta 1e-3 1.0 1.0 nil)))
                      (if f-e (mk-ae-step grad-i caddr emm-delta 0 max-d 0.0 nil))
                      (funcall bar-cb 0.95)))))))

          (if (> delta 1.0) (setf delta (* 1.1 delta)))
          (setf (slot-value s 'delta) delta)
          (values (slot-value s 'matrix)
                  (chi (source s) s)
                  delta
                  (loop for p in profs collect (cur-i p s))
                  (absorbtion-profile s)
                  (gauss-abs-cfs s)))))))


(defmethod-f perform-step ((s art-solver) bar-cb cb)
  (register-main-lambda bar-cb)
  (funcall bar-cb 0.01)
  (bind-exit-values-for (stp m chi delta iis adp adelta idelta gcfs)
    (run-in-web-worker (wrk s)
      (cache-vars t)
      (let ((tim0 ((jscl::oget (jscl::make-new (winref "Date")) "getTime")))
            (flg nil)
            (n 0)
            (delta-sum 0)
            (adelta-sum 0)
            (idelta-sum 0)
            (iter-lim (iteration-limit s))
            res)
        (loop until flg do
          (let ((tim1 ((jscl::oget (jscl::make-new (winref "Date")) "getTime"))))
            (setf res (multiple-value-list (calc-step s bar-cb)))
            (setf (slot-value s 'cur-step) (1+ (cur-step s)))
            (setf flg (or (< (delta s) 1)
                          (> (- tim1 tim0) 10000)
                          (and (> iter-lim 0) (cur-step s) (>= (cur-step s) iter-lim))))
            (incf n)
            (incf delta-sum (delta s))
            (incf adelta-sum (ads-delta s))
            (incf idelta-sum (emm-delta s))))
        (destructuring-bind (m chi delta cur-i adsp gcfs) res
          (values (cur-step s) m chi (/ delta-sum n) cur-i adsp (/ adelta-sum n) (/ idelta-sum n) gcfs))))
    (progn
      (funcall bar-cb 1)
      (let ((m0 (matrix s)))
        (loop for i below (apply #'* (array-dimensions m)) do (setf (aref m0 i) (aref m i))))
      (setf (slot-value s 'solver-chi) chi)
      (setf (slot-value s 'delta) delta)
      (setf (slot-value s 'cur-step) stp)
      (setf (slot-value s 'absorbtion-profile) adp)
      (setf (slot-value s 'ads-delta) adelta)
      (setf (slot-value s 'emm-delta) idelta)
      (setf (slot-value s 'gauss-abs-cfs) gcfs)
      (loop for p in (profiles (source s)) and i in iis do (setf (slot-value p 'cur-i) i))
      (funcall cb))))


(defmethod-f initialize-instance :after ((s solver-widget) &key source params &allow-other-keys)
  (setf (slot-value s 'solver) (make-instance 'art-solver :source source :params (params s))))

(defmethod-f update-params ((s solver-widget) (p parameters))
  (if (slot-boundp (solver s) 'wrk) (kill (wrk (solver s))))
  (setf (slot-value s 'solver) (make-instance 'art-solver :source (source (solver s)) :params (params s)))
  (redraw s))

(defclass-f informer (omg-widget)
  ((solver :initarg :solver
           :accessor solver)
   (last-chi :initform nil
             :accessor last-chi)))

(defmethod-f render-widget ((i informer))
  (setf (slot-value i 'root)
        (create-element "table" :|style.width| "100%"
                                :|cellpadding| "0.2em"
          :append-element
            (create-element "tr"
              :append-element
                (create-element "td" :|align| "right"
                                     :|valign| "bottom"
                                     :|innerHTML| "Step:")
              :append-element
                (create-element "td" :|align| "left"
                                     :|valign| "bottom"
                                     :|innerHTML| (if (cur-step (solver i))
                                                      (if (> (cur-step (solver i)) 0)
                                                          (format nil "#~A" (cur-step (solver i)))
                                                          "initialization...")
                                                      "")))
          :append-element
            (create-element "tr"
              :append-element
                (create-element "td" :|align| "right"
                                     :|valign| "bottom"
                                     :|style.fontFamily| "serif"
                                     :|innerHTML| "&Delta;<sub>tom</sub>:")
              :append-element
                (create-element "td" :|align| "left"
                                     :|valign| "bottom"
                                     :|innerHTML| ((jscl::oget (delta (solver i)) "toPrecision") 4)))
          :append-element
            (create-element "tr"
              :append-element
                (create-element "td" :|align| "right"
                                     :|valign| "bottom"
                                     :|style.fontFamily| "serif"
                                     :|innerHTML| "&Delta;<sub>abs</sub>:")
              :append-element
                (create-element "td" :|align| "left"
                                     :|valign| "bottom"
                                     :|innerHTML| ((jscl::oget (ads-delta (solver i)) "toPrecision") 4)))
          :append-element
            (create-element "tr"
              :append-element
                (create-element "td" :|align| "right"
                                     :|valign| "bottom"
                                     :|style.fontFamily| "serif"
                                     :|innerHTML| "&Delta;<sub>emm</sub>:")
              :append-element
                (create-element "td" :|align| "left"
                                     :|valign| "bottom"
                                     :|innerHTML| ((jscl::oget (emm-delta (solver i)) "toPrecision") 4)))
          :append-element
            (create-element "tr"
              :append-element
                (create-element "td" :|align| "right"
                                     :|style.verticalAlign| "middle"
                                     :|style.height| "2em"
                                     :|style.fontFamily| "serif"
                                     :|innerHTML| "&chi;<sup>2</sup>:")
              :append-element
                (create-element "td" :|align| "left"
                                     :|valign| "bottom"
                                     :|style.height| "2em"
                                     :|style.verticalAlign| "middle"
                                     :|innerHTML|
                                       (prog1
                                         (if (slot-boundp (solver i) 'solver-chi)
                                             (format nil "~A~A"
                                               ((jscl::oget (solver-chi (solver i)) "toPrecision") 3)
                                               (let ((dl (if (and (last-chi i)
                                                                  (cur-step (solver i))
                                                                  (> (cur-step (solver i)) 0))
                                                             (- (solver-chi (solver i)) (last-chi i)))))
                                                 (setf (slot-value i 'last-chi) (solver-chi (solver i)))
                                                 (if dl
                                                     (format nil " (~A)" ((jscl::oget dl "toPrecision") 3))
                                                     "")))
                                             "")))))))

(defmethod-f render-widget :after ((z zoomed-images))
  (let* ((ctrls (cons (root (img z)) (ctrls z)))
         (parents (mapcar #'parent-element ctrls))
         (prevs (mapcar (lambda (ctrl) (jscl::oget ctrl "nextSibling")) ctrls)))
    (append-element
      (create-element "div" :|style.width| (* 0.8 (page-width))
                            :|style.maxHeight| (* 0.9 (page-height))
                            :|style.background| "white"
                            :|style.border| "1px solid black"
                            :|style.borderRadius| "0.5em"
                            :|style.padding| "1em"
                            :|style.display| "flex"
                            :|style.flexFlow| "column wrap"
        :append-elements
          (loop for (e1 e2) on ctrls by #'cddr collect
            (create-element "div" :|style.width| "100%"
                                  :|style.display| "flex"
                                  :|style.flexFlow| "row wrap"
               :append-element (create-element "div" :|style.display| "inline-block"
                                                     :|style.flex| 1
                                  :append-element e1)
               :append-elements (if e2 (list (create-element "div" :|style.display| "inline-block"
                                                                   :|style.flex| 1
                                               :append-element e2)))))
        :append-element (create-element "button" :|innerHTML| "close"
                                                 :|style.position| "absolute"
                                                 :|style.top| "0.5em"
                                                 :|style.right| "0.5em"
                          :|onclick| (lambda (ev)
                                       (map nil (lambda (el parent prev)
                                                  ((jscl::oget parent "insertBefore") el prev))
                                                (reverse ctrls)
                                                (reverse parents)
                                                (reverse prevs))
                                       (close z))))
      (root z))))



(defmethod-f render-widget ((s solver-widget))
  (setf (slot-value s 'root)
        (labels ((get-adsp-data ()
                   (let* ((adsp (absorbtion-profile (solver s)))
                          (ads-data (map 'list (lambda (x) (cons (car x) (cadr x))) adsp))
                          (emm-data (map 'list (lambda (x) (cons (car x) (+ 1 (caddr x)))) adsp))
                          (sum-data (mapcar (lambda (x y) (cons (car x) (+ (cdr x) (cdr y) -1))) ads-data emm-data)))
                     (values ads-data
                             emm-data
                             sum-data
                             (- (apply #'min (mapcar #'cdr ads-data)) 0.1)
                             (+ (apply #'max (mapcar #'cdr emm-data)) 0.1))))
                 (save-text-data (name data)
                   (let* ((blob (jscl::make-new  (winref "Blob")
                                     (jscl::make-new (winref "Array") (jscl::lisp-to-js data))
                                     (make-js-object :|type| "text/plain")))
                          (url ((jscl::oget (jscl::lisp-to-js (jscl::%js-vref "URL")) "createObjectURL") blob))
                          (el (create-element "a" :|href| url
                                                  :|download| name)))
                     (append-element el)
                     ((jscl::oget el "click"))
                     (remove-element el)
                     ((jscl::oget (jscl::lisp-to-js (jscl::%js-vref "URL")) "revokeObjectURL") url))))
          (let* ((max-v (max-v (params s)))
                 (img (make-instance 'saveable-graph :xmin (- max-v) :xmax max-v
                                                     :ymin (- max-v) :ymax max-v
                                                     :scales '(:left :right :top :bottom)
                                                     :preserve-aspect-ratio t
                                                     :xcaption "V (km/s)"
                                                     :ycaption (create-element "span" :|style.whiteSpace| "nowrap" :|innerHTML| "V (km/s)")
                                                     :name (format nil "~A tomogram" (name (source (solver s))))))
                 (ctrls (get-controls (source (solver s)) (graph img) img (matrix (solver s))))
                 (plt (make-instance 'matrix-plot :matrix (matrix (solver s)) :norm t
                                                  :xmin (xmin img) :xmax (xmax img)
                                                  :ymin (ymin img) :ymax (ymax img)))
                 (inf (make-instance 'informer :solver (solver s)))
                 (pgb (make-instance 'progress-bar :width "100%"
                                                   :bg-style '(:|style.border| "1px solid black"
                                                               :|style.background| "white"
                                                               :|style.visibility| "hidden")))
                 (state-on "Start image reconstruction")
                 (state-off "Stop reconstruction"))
            (multiple-value-bind (ads-data emm-data sum-data ads-min emm-max) (get-adsp-data)
              (let ((pcyg-grf (make-instance 'saveable-graph :xmin (- max-v) :xmax max-v
                                                             :ymin ads-min
                                                             :ymax emm-max
                                                             :scales '(:left :bottom)
                                                             :xcaption "V (km/s)"
                                                             :ycaption (create-element "span" :|style.whiteSpace| "nowrap"
                                                                                              :|style.fontFamily| "serif"
                                                                                              :|innerHTML| "I")
                                                             :name (format nil "~A abs-emm lines" (name (source (solver s))))))
                    (ads-plt (make-instance 'tabular-plot :table ads-data :color "blue"))
                    (emm-plt (make-instance 'tabular-plot :table emm-data :color "red")))
                    ; (sum-plt (make-instance 'tabular-plot :table sum-data :color "black")))
                (add-plot img plt)
                (add-plot pcyg-grf ads-plt)
                (add-plot pcyg-grf emm-plt)
                ; (add-plot pcyg-grf sum-plt)

                (create-element "div" :|style.width| "20em"
                                      :|style.textAlign| "center"
                                      :|style.float| "left"
                  :append-element (render-widget img)
                  :append-element (create-element "div" :|style.marginTop| "1em"
                                    :append-element (create-element "button" :|innerHTML| "zoom"
                                                      :|onclick| (lambda (ev)
                                                                   (append-element
                                                                     (render-widget
                                                                       (make-instance 'zoomed-images :img img :ctrls ctrls))))))
                  :append-element (create-element "div" :|style.marginTop| "0.5em"
                                    :append-elements ctrls)
                  :append-element
                    (create-element "div" :|style.marginTop| "1em"
                      :append-element (create-element "a" :|href| "#"
                                                          :|innerHTML| "save image data"
                                        :|onclick| (lambda (ev)
                                                     (save-text-data (format nil "~A_image.dat" (name (source (solver s))))
                                                       (apply #'concatenate
                                                         (cons 'string
                                                           (let ((m (matrix (solver s))))
                                                             (destructuring-bind (nx ny) (array-dimensions m)
                                                               (loop for j below ny append
                                                                 (let ((vy (* -2 max-v (- (/ j (1- ny)) 0.5))))
                                                                   (loop for i below nx collect
                                                                     (let ((vx (* 2 max-v (- (/ i (1- nx)) 0.5))))
                                                                         (format nil "~A ~A ~A~%" vx vy (aref m (+ i (* nx j))))))))))))))))
                  :append-element (create-element "div" :|style.height| "15em"
                                                        :|style.width| "15em"
                                                        :|style.display| "inline-block"
                                                        :|style.marginTop| "1em"
                                    :append-element (render-widget pcyg-grf))
                  :append-element
                    (create-element "div" :|style.marginTop| "1em"
                      :append-element (create-element "a" :|href| "#"
                                                          :|innerHTML| "save absorbtion line"
                                        :|onclick| (lambda (ev)
                                                     (save-text-data (format nil "~A_absorbtion.dat" (name (source (solver s))))
                                                       (apply #'concatenate
                                                         (cons 'string
                                                           (multiple-value-bind (ads-data emm-data) (get-adsp-data)
                                                               (loop for d in ads-data when (and (>= (car d) (- max-v))
                                                                                                 (<= (car d) max-v))
                                                                 collect
                                                                 (format nil "~A ~A~%" (car d) (cdr d))))))))))
                  :append-element
                    (create-element "div" :|style.marginTop| "1em"
                      :append-element (create-element "a" :|href| "#"
                                                          :|innerHTML| "save emission line"
                                        :|onclick| (lambda (ev)
                                                     (save-text-data (format nil "~A_emission.dat" (name (source (solver s))))
                                                       (apply #'concatenate
                                                         (cons 'string
                                                           (multiple-value-bind (ads-data emm-data) (get-adsp-data)
                                                               (loop for d in emm-data when (and (>= (car d) (- max-v))
                                                                                                 (<= (car d) max-v))
                                                                 collect
                                                                 (format nil "~A ~A~%" (car d) (cdr d))))))))))
                  :append-element
                    (create-element "div" :|style.width| "80%"
                                          :|style.display| "inline-block"
                                          :|style.marginTop| "1em"
                      :append-element (render-widget pgb))
                  :append-element (render-widget inf)
                  :append-elements (render-config (solver s))
                  :append-elements
                    (labels ((remake-all (&key no-blue)
                               (reset (source (solver s)))
                               (multiple-value-bind (ads-data emm-data sum-data ads-min emm-max) (get-adsp-data)
                                 (setf (slot-value ads-plt 'table) ads-data)
                                 (setf (slot-value emm-plt 'table) emm-data)
                                 (setf (slot-value pcyg-grf 'ymin) ads-min)
                                 (setf (slot-value pcyg-grf 'ymax) emm-max)
                                 (redraw pcyg-grf))
                               (map nil (lambda (p)
                                          (update-profile-plots p (source (solver s)) :show-cur-i (if (not no-blue) (solver s))))
                                        (profiles (source (solver s))))
                               (redraw (trail (source (solver s))))
                               (redraw inf)
                               (redraw plt)))
                      (list (create-element "button" :|style.width| "80%"
                                                     :|style.marginTop| "1em"
                                                     :|innerHTML| "Reset absorbtion"
                              :|onclick| (lambda (ev)
                                           (map nil
                                                (lambda (v)
                                                  (setf (cadr v) 1.0))
                                                (absorbtion-profile (solver s)))
                                           (remake-all)))
                            (create-element "button" :|style.width| "80%"
                                                     :|style.marginTop| "1em"
                                                     :|innerHTML| "Reset emission"
                              :|onclick| (lambda (ev)
                                           (map nil
                                                (lambda (v)
                                                  (setf (caddr v) 0.0))
                                                (absorbtion-profile (solver s)))
                                           (remake-all)))
                            (create-element "button" :|style.width| "80%"
                                                     :|style.marginTop| "1em"
                                                     :|innerHTML| "Reset solver"
                              :|onclick| (lambda (ev)
                                           (reset (solver s))
                                           (remake-all :no-blue t)))))
                  :append-element
                    (with-self but
                      (create-element "button" :|style.width| "80%"
                                               :|style.marginTop| "1em"
                                               :|innerHTML| state-on
                        :|onclick| (let ((state nil))
                                     (lambda (ev)
                                       (labels ((mk-step ()
                                                  (set-progress pgb 0)
                                                  (perform-step
                                                    (solver s)
                                                    (lambda (v) (set-progress pgb v))
                                                    (lambda ()
                                                      (execute-after 0
                                                        (lambda ()
                                                          (redraw plt)
                                                          (redraw inf)
                                                          (let ((src (source (solver s)))
                                                                (m (matrix (solver s))))
                                                            (with-no-updates (trail src)
                                                              (map nil (lambda (p)
                                                                         (update-profile-plots p src :show-cur-i (solver s)))
                                                                       (profiles src)))
                                                            ;;(map nil #'redraw (mapcar #'cur-plot (profiles src)))
                                                            (multiple-value-bind (ads-data emm-data sum-data ads-min emm-max) (get-adsp-data)
                                                              (setf (slot-value ads-plt 'table) ads-data)
                                                              (setf (slot-value emm-plt 'table) emm-data)
                                                              (setf (slot-value pcyg-grf 'ymin) ads-min)
                                                              (setf (slot-value pcyg-grf 'ymax) emm-max)
                                                              (redraw pcyg-grf)
                                                              (redraw (trail src))))))
                                                      (if state
                                                          (if (and (> (delta (solver s)) 1.0)
                                                                   (or (equal (iteration-limit (solver s)) 0)
                                                                       (< (cur-step (solver s)) (iteration-limit (solver s)))))
                                                              (mk-step)
                                                              (progn
                                                                (setf state nil)
                                                                (setf (jscl::oget but "innerHTML") state-on)
                                                                (setf (jscl::oget (root pgb) "style" "visibility") "hidden"))))))))
                                         (if (and (profiles (source (solver s))) (setf state (not state)))
                                             (progn
                                               (setf (jscl::oget (root pgb) "style" "visibility") "visible")
                                               (let ((s (solver s)))
                                                 (if (not (cur-step s))
                                                     (progn
                                                       (reset s)
                                                       (setf (slot-value s 'cur-step) 0))
                                                     (progn
                                                       (if (slot-boundp s 'wrk) (kill (wrk s)))
                                                       (setf (slot-value s 'wrk) (make-instance 'classic-worker :persistent-cache t)))))
                                               (setf (jscl::oget but "innerHTML") state-off)
                                               (setf (slot-value (solver s) 'delta) 10000)
                                               (redraw inf)
                                               (let* ((s (source (solver s)))
                                                      (profs (profiles s))
                                                      (profs `(,@(last profs) ,@profs ,(car profs)))
                                                      (phases (mapcar #'phase profs))
                                                      (phases `(,(- (car phases) 1)
                                                                ,@(subseq (cdr phases) 0 (- (length phases) 2))
                                                                ,(+ (car (last phases)) 1))))
                                                 (loop for pl on profs and phl on phases
                                                   when (cddr pl) do
                                                   (setf (slot-value (cadr pl) 'phase-weight)
                                                         (* 0.5 (- (caddr phl) (car phl)))))
                                                (mk-step)))
                                             (progn
                                               (if (wrk (solver s)) (kill (wrk (solver s))))
                                               (setf (jscl::oget (root pgb) "style" "visibility") "hidden")
                                               (setf (jscl::oget but "innerHTML") "Continue reconstruction")))))))))))))))
