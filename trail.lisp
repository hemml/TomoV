(in-package :tomo)

(defun-f switch-plots (src plt mode)
  (map nil (lambda (p)
             (when p
               (with-slots (root) p
                 (when root
                   (setf (jscl::oget root "style" "visibility") (if mode "visible" "hidden"))))))
           (mapcar plt (profiles src))))

(defclass-conf trail-graph (saveable-graph watcher)
 ((xcaption :initform (create-element "span" :|style.whiteSpace| "nowrap" :|innerHTML| "Vx (km/s)")
            :accessor xcaption)
  (ycaption :initform "Phase"
            :accessor ycaption)
  (ymin :initform -0.1)
  (ymax :initform 1.1)
  (yticks :initform 20
          :accessor yticks)
  (show-scales :initform '(:left :top :right :bottom))
  (curv-cf  :initform 0.1
            :accessor curv-cf
            :desc "Scale"
            :type :number
            :validator (lambda (v) (> v 0))
            :onchange (lambda (g)
                        ; (with-no-updates g
                        (with-slots (source) g
                          (map nil (lambda (p) (update-profile-plots p source)) (profiles source))
                          (redraw g))))
  (show-observ :initform t
               :desc "Show observational profiles"
               :type :checkbox
               :onchange (lambda (trail)
                           (with-slots (show-observ source) trail
                             (switch-plots source #'plot show-observ))))
  (show-synth :initform t
               :desc "Show synthetic profiles"
               :type :checkbox
               :onchange (lambda (trail)
                           (with-slots (show-synth source) trail
                             (switch-plots source #'cur-plot show-synth))))
  (show-den :initform t
            :desc "Show denoised profiles"
            :type :checkbox
            :onchange (lambda (trail)
                        (with-slots (show-den source) trail
                          (switch-plots source #'den-plot show-den))))
  (source :accessor source
          :initarg :source)
  (name :initform "Trail spectra")
  (no-cf-updates :initform nil)))

(defmethod-f redraw :after ((g trail-graph)))

(defmethod-f render-widget :after ((g trail-graph))
  (with-slots (root show-synth show-observ show-den source) g
    (ensure-element root
      (switch-plots source #'plot show-observ)
      (switch-plots source #'cur-plot show-synth)
      (switch-plots source #'den-plot show-den))))


(defclass-f trail-profile (profile)
  ((mcl :initform nil)
   (next-prof :initform nil)))

(defclass-f trail-plot (tabular-plot)
  ((phase :initarg :phase
          :accessor phase)))

(defmethod-f xmin ((g trail-graph))
  (- (max-v (params g))))

(defmethod-f xmax ((g trail-graph))
  (max-v (params g)))

(defmethod-f table ((p trail-plot))
  (with-slots (parent table phase) p
    (let ((cf (curv-cf parent))
          (ofs (offset (source parent))))
      (mapcar
        (lambda (xy)
          (cons (car xy)
                (+ phase (* cf (- (cdr xy) ofs)))))
        table))))


(defmethod-f curv-cf ((g trail-graph))
  (with-slots (curv-cf source no-cf-updates) g
    (when (and (not curv-cf) (not no-cf-updates))
      (with-accessors ((xmin xmin) (xmax xmax)) g
        (labels ((flt (p)
                   (remove-if (lambda (x)
                                (or (< (car x) xmin)
                                    (> (car x) xmax)))
                              (data p))))
          (let ((profiles (profiles source)))
            (when profiles
              (let* ((s-profs (sort profiles #'< :key #'phase))
                     (ofs (offset source))
                     (delta 0.005)
                     (cfmax (/ 0.1 (- (apply #'max (mapcar #'max-d profiles))
                                      ofs)))
                     (mcl (loop for (p1 p2) on s-profs when p2 collect
                            (let ((dp (- (phase p2) (phase p1) delta))
                                  (t1 (flt p1))
                                  (t2 (flt p2))
                                  cur-t2
                                  res)
                              (change-class p1 'trail-profile)
                              (with-slots (mcl next-prof) p1
                                (when (or (not mcl) (not (equal p2 next-prof)))
                                  (loop while (and t1 t2) do
                                     (let* ((p1 (car t1))
                                            (p2 (car t2))
                                            (x1 (car p1))
                                            (x2 (car p2))
                                            (y1 (- (cdr p1) ofs))
                                            (y2 (- (cdr p2) ofs)))
                                       (if (<= x2 x1)
                                           (setf t2 (cdr t2)
                                                 cur-t2 y2)
                                           (if cur-t2
                                               (setf t1 (cdr t1)
                                                     res (if (> y1 cur-t2)
                                                             (let ((r (/ dp (- y1 cur-t2))))
                                                               (if res
                                                                   (min res r)
                                                                   r))
                                                             res))
                                               (setf t1 (cdr t1))))))
                                  (setf mcl res
                                        next-prof p2))
                                mcl)))))
                (setf curv-cf (apply #'min (cons cfmax (remove-if #'null mcl))))))))))
    (if curv-cf
        curv-cf
        1)))

(def-local-macro-f with-no-updates (trail &rest code)
  (let ((res (gensym))
        (ncf (gensym))
        (ccf (gensym)))
    `(with-slots ((,ncf no-cf-updates) (,ccf curv-cf)) ,trail
       (setf ,ncf t)
       (let ((,res (progn ,@code)))
         (setf ,ncf nil)
         ,res))))

(defmethod-f make-curve ((p trail-plot))
  (let* ((g (parent p))
         (ymin (ymin g))
         (ymax (ymax g))
         (tbl (table p)))
    `(g (path :|stroke| ,(color p)
              :|stroke-width| "1px"
              :|vector-effect| "non-scaling-stroke"
              :|fill| "none"
              :|d| ,(apply #'jscl::concat
                           (cons (jscl::concat "M" (jscl::float-to-string (caar tbl)) ","
                                                   (jscl::float-to-string (+ ymin (- ymax (cdar tbl)))) " ")
                                 (mapcar (lambda (i)
                                           (jscl::concat "L" (jscl::float-to-string (car i)) ","
                                                             (jscl::float-to-string (+ ymin (- ymax (cdr i)))) " "))
                                         (cdr tbl))))))))



(defmethod-f add-plot :after ((g trail-graph) (p trail-plot))
  (with-slots (curv-cf no-cf-updates) g
    (when (not no-cf-updates)
      (redraw g))))

(defmethod-f remove-all-plots :after ((g trail-graph)))
