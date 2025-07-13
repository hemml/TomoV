(in-package :tomo)

(defclass-f trail-graph (saveable-graph watcher)
  ((xcaption :initform (create-element "span" :|style.whiteSpace| "nowrap" :|innerHTML| "Vx (km/s)")
             :accessor xcaption)
   (ycaption :initform "Phase"
             :accessor ycaption)
   (ymin :initform 0)
   (ymax :initform 1.1)
   (yticks :initform 20
           :accessor yticks)
   (show-scales :initform '(:left :top :right :bottom))
   (curv-cf  :initform nil
             :accessor curv-cf)
   (source :accessor source
           :initarg :source)
   (name :initform "Trail spectra")))

(defclass-f trail-plot (tabular-plot)
  ((phase :initarg :phase
          :accessor phase)))

(defmethod-f xmin ((g trail-graph))
  (- (max-v (params g))))

(defmethod-f xmax ((g trail-graph))
  (max-v (params g)))

(defmethod-f table ((p trail-plot))
  (with-slots (parent) p
    (let ((cf (curv-cf parent))
          (phase (phase p))
          (ofs (offset (source parent))))
      (mapcar
        (lambda (xy)
          (cons (car xy)
                (+ phase (* cf (- (cdr xy) ofs)))))
        (slot-value p 'table)))))

(defmethod-f curv-cf ((g trail-graph))
  (with-slots (curv-cf source) g
    (when (not curv-cf)
      (let* ((profiles (profiles source))
             (s-profs (sort profiles #'< :key #'phase))
             (xmin (xmin g))
             (xmax (xmax g))
             (ofs (offset source))
             (delta 0.02)
             (cfmax (/ 0.1 (- (apply #'max (mapcar #'cdr (remove-if (lambda (p))
                                                                (or (< (car p) xmin)
                                                                    (> (car p) xmax))
                                                              (apply #'append (mapcar #'data profiles)))))
                              ofs)))
             (mcl (loop for (p1 p2) on s-profs when p2 collect
                    (let ((dp (- (phase p2) (phase p1) delta)))
                      (labels ((flt (p)
                                 (remove-if (lambda (x)
                                              (or (< (car x) xmin)
                                                  (> (car x) xmax)))
                                            (data p)))
                               (lp (&optional (t1 (flt p1)) (t2 (flt p2)) cur-t2 res)
                                 (if (and t1 t2)
                                     (let* ((p1 (car t1))
                                            (p2 (car t2))
                                            (x1 (car p1))
                                            (x2 (car p2))
                                            (y1 (- (cdr p1) ofs))
                                            (y2 (- (cdr p2) ofs)))
                                       (jslog x1 y1 x2 y2 cur-t2 res)
                                       (if (<= x2 x1)
                                           (lp t1 (cdr t2) y2 res)
                                           (lp (cdr t1)
                                               t2
                                               cur-t2
                                               (if (> y1 cur-t2)
                                                   (let ((r (/ dp (- y1 cur-t2))))
                                                     (if res
                                                         (min res r)
                                                         r))
                                                   res))))
                                     res)))
                        (lp))))))
        (setf curv-cf (apply #'min (cons cfmax (remove-if #'null mcl))))))
    curv-cf))

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
                           (cons (format nil "M~A,~A " (caar tbl) (+ ymin (- ymax (cdar tbl))))
                                 (loop for i in (cdr tbl) collect
                                   (format nil "L~A,~A " (car i) (+ ymin (- ymax (cdr i)))))))))))

(defmethod-f add-plot :after ((g trail-graph) (p trail-plot))
  (with-slots (curv-cf) g
    (with-slots (phase table) p
      (let* ((mv (max-v (params g)))
             (ofs 1))
        (setf curv-cf nil)
        (redraw g)))))

(defmethod-f remove-all-plots :after ((g trail-graph))
  (with-slots (curv-cf) g
    (setf cur-cf nil)))
