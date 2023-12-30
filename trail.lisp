(in-package :tomo)

(defclass-f trail-graph (graph watcher)
  ((xcaption :initform (create-element "span" :|style.whiteSpace| "nowrap" :|innerHTML| "Vx (km/s)")
             :accessor xcaption)
   (ycaption :initform "Phase"
             :accessor ycaption)
   (ymin :initform 0)
   (ymax :initform 1.2)
   (yticks :initform 20
           :accessor yticks)
   (show-scales :initform '(:left :top :right :bottom))
   (last-max :initform nil
             :accessor last-max)))

(defclass-f trail-plot (tabular-plot)
  ((phase :initarg :phase
          :accessor phase)))

(defmethod-f xmin ((g trail-graph))
  (- (max-v (params g))))

(defmethod-f xmax ((g trail-graph))
  (max-v (params g)))

(defmethod-f table ((p trail-plot))
  (let ((mx (if (last-max (parent p))
                (last-max (parent p))
                1))
        (phase (phase p)))
    (mapcar
      (lambda (xy)
        (cons (car xy)
              (+ phase (* 0.2 (/ (cdr xy) mx)))))
      (slot-value p 'table))))

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
                                   (format nil "L~A,~A " (car i) (+ ymin (- ymax (cdr i)))))))
              :|onmouseover| ,(lambda (ev)
                                (jslog "OVER!" ev))
                                ; (setf (jscl::oget ev "target" "style" "strokeWidth") "3px"))
              :|onmouseenter| ,(lambda (ev)
                                 (jslog "ENTER!" ev))
              :|onclick| ,(lambda (ev)
                            (jslog "CLICK!" ev))
              :|onmousemove| ,(lambda (ev)
                                (jslog "MOVE!" ev))
              :|onmouseleave| ,(lambda (ev)
                                 (jslog "LEAVE!" ev))
              :|onmouseout| ,(lambda (ev)
                               (jslog "OUT!" ev))))))
                               ; (setf (jscl::oget ev "target" "style" "strokeWidth") "1px"))))))
        ; (path :|stroke| "blue"
        ;       :|stroke-width| "5px"
        ;       :|vector-effect| "non-scaling-stroke"
        ;       :|fill| "none"
        ;       :|d| ,(apply #'jscl::concat
        ;                    (cons (format nil "M~A,~A " (caar tbl) (+ ymin (- ymax (cdar tbl))))
        ;                          (loop for i in (cdr tbl) collect
        ;                            (format nil "L~A,~A " (car i) (+ ymin (- ymax (cdr i)))))))

(defmethod-f add-plot :after ((g trail-graph) (p trail-plot))
  (let* ((mx (apply #'max (mapcar #'cdr (slot-value p 'table)))))
    (if (or (not (last-max g))
            (> mx (* 1.1 (last-max g))))
        (progn
          (setf (slot-value g 'last-max) mx)
          (redraw g)))))
