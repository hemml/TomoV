(in-package :tomo)

(defclass-f sample-profile-source (profile-source-widget)
  ((npoints)
   (snr :accessor snr
        :initform 10)
   (picker :accessor picker)
   (sample-char :accessor sample-char
                :initform "A")))

(defclass-f sample-picker (graph watcher)
  ((matrix :accessor matrix)
   (normal-plot)
   (blured-plot)
   (preserve-aspect-ratio :initform t)
   (cur-phase :accessor cur-phase
              :initform nil)
   (sample-char :accessor sample-char
                :initarg :sample-char)))

(defclass-f blr-plot (matrix-plot watcher)
  ((done :initform nil
         :accessor done)))

(defclass-f get-prof-dialog (modal-dialog-window)
  ((stop-cb :accessor stop-cb)
   (bar :accessor bar)))

(defclass-f get-prof-progress (modal-dialog-window)
  ((bar :accessor bar)
   (stop-cb :accessor stop-cb
            :initarg :stop-cb
            :initform (lambda (&rest args)))))

(lazy-slot npoints ((s sample-profile-source))
  (jsfloor (* 1.5 (resolution (params s)))))

(defmethod-f update-params ((s sample-profile-source) (p parameters))
  (setf (slot-value s 'npoints) (jsfloor (* (resolution p) 1.5)))
  (redraw s))

(lazy-slot normal-plot ((g sample-picker))
  (make-instance 'matrix-plot :matrix (matrix g) :norm t
                 :xmin (xmin g) :xmax (xmax g)
                 :ymin (ymin g) :ymax (ymax g)))

(defmethod-f render-widget :after ((p blr-plot))
  (if (not (done p))
      (let* ((pb (make-instance 'progress-bar :bg-style '(:|style.border| "1px solid black"
                                                          :|style.background| "white"
                                                          :|style.top|  "50%"
                                                          :|style.left| "50%"
                                                          :|style.transform| "translate(-50%, -50%)")
                                              :width "80%"))
             (bg (create-element "div" :|style.position| "absolute"
                                       :|style.top| 0
                                       :|style.bottom| 0
                                       :|style.left| 0
                                       :|style.right| 0
                                       :|style.background| "white"
                   :append-element (render-widget pb))))
        (ensure-element (root p)
          (append-element bg (parent-element (root p))))
        (let ((fwhm (fwhm (params p)))
              (max-v (max-v (params p)))
              (nx (resolution (params p)))
              (ny (resolution (params p)))
              (odat (matrix p))
              (upd-progress (lambda (c) (set-progress pb c)))
              (ww (make-instance 'classic-worker :persistent-cache t)))
          (register-main-lambda upd-progress)
          (bind-exit-values-for (bdat)
            (run-in-web-worker ww
              (let* ((tim0 ((jscl::oget (jscl::make-new (winref "Date")) "getTime")))
                     (pcnt 0)
                     (acnt 0)
                     (a (sqr (* 2 (/ (jsln 2) fwhm))))
                     (cf (* (/ 2 fwhm) (sqrt (/ (jsln 2) pi))))
                     (bdat (make-array (list nx ny)))
                     (rd (loop for i below nx when
                           (let* ((x (/ i (- nx 1)))
                                  (v (exp (* -1 a (* (sqr (* max-v x)))))))
                             (< v 0.05))
                           return i)))
                (loop for i below nx do
                  (let ((x (- (* 2.0 (/ i (- nx 1))) 1.0)))
                    (loop for j below ny do
                      (let ((y (- (* 2.0 (/ j (- ny 1))) 1.0)))
                        (if (> (incf pcnt) 100)
                           (let ((tim1 ((jscl::oget (jscl::make-new (winref "Date")) "getTime"))))
                             (setf pcnt 0)
                             (when (> (- tim1 tim0) 200)
                               (funcall upd-progress (/ acnt (* nx ny)))
                               (setf tim0 tim1))))
                        (incf acnt)
                        (setf (aref bdat (+ i (* nx j)))
                              (loop for i1 from (max 0 (- i rd)) below (min nx (+ i rd)) sum
                                (let ((x1 (- (* 2.0 (/ i1 (- nx 1))) 1.0)))
                                  (loop for j1 from (max 0 (- j rd)) below (min ny (+ j rd)) sum
                                    (let ((y1 (- (* 2.0 (/ j1 (- ny 1))) 1.0)))
                                      (* (aref odat (+ i1 (* j1 nx)))
                                         cf
                                         (exp (* -1 a (* (sqr max-v) (+ (sqr (- x x1)) (sqr (- y y1))))))))))))))))
                bdat))
            (setf (slot-value p 'matrix) bdat)
            (setf (slot-value p 'done) t)
            (remove-element bg)
            (redraw p))))))

; (print
;   (macroexpand-1))
(lazy-slot blured-plot ((g sample-picker))
  (make-instance 'blr-plot :matrix (matrix g) :norm t
                 :xmin (xmin g) :xmax (xmax g)
                 :ymin (ymin g) :ymax (ymax g)
                 :params (params g)))

(defmethod-f render-widget :after ((g sample-picker))
  (let* ((nx (resolution (params g)))
         (ny (resolution (params g)))
         (ch (sample-char g))
         (cnv1 (create-element "canvas" :|width| nx
                                        :|height| ny))
         (ctx1 ((jscl::oget cnv1 "getContext") "2d")))
    (setf (jscl::oget ctx1 "font") (format nil "~Apx serif" nx))
    (let* ((m ((jscl::oget ctx1 "measureText") ch))
           (mw (jscl::oget m "width"))
           (mh (jscl::oget m "actualBoundingBoxAscent"))
           (phi-div (create-element "div" :|style.position| "absolute"
                                          :|style.left| 0
                                          :|style.top| 0
                                          :|style.background| "yellow"
                                          :|style.fontSize| "0.75em"
                                          :|style.width| "3.5em"
                                          :|style.fontFamily| "serif"
                                          :|innerHTML| "&#966;=0"
                                          :|style.zIndex| 20
                                          :|style.margin| "5px"
                                          :|style.visibility| "hidden"))
           (arrow (make-svg :|viewBox| "0 0 400 400"
                            '(:line :|x1| 200 :|y1| 600
                                    :|x2| 200 :|y2| -200
                                    :|stroke| "white"
                                    :|opacity| "0.5"
                                    :|stroke-width| 5)
                            '(:line :|x1| 190 :|y1| 230
                                    :|x2| 200 :|y2| 200
                                    :|stroke| "white"
                                    :|opacity| "0.5"
                                    :|stroke-width| 5)
                            '(:line :|x1| 210 :|y1| 230
                                    :|x2| 200 :|y2| 200
                                    :|stroke| "white"
                                    :|opacity| "0.5"
                                    :|stroke-width| 5)
                            '(:line :|x1| 200 :|y1| 600
                                    :|x2| 200 :|y2| -200
                                    :|stroke| "red"
                                    :|stroke-width| 2
                                    :|stroke-dasharray| 10)
                            '(:line :|x1| 190 :|y1| 230
                                    :|x2| 200 :|y2| 200
                                    :|stroke| "red"
                                    :|stroke-width| 2)
                            '(:line :|x1| 210 :|y1| 230
                                    :|x2| 200 :|y2| 200
                                    :|stroke| "red"
                                    :|stroke-width| 2)
                            :|style.position| "absolute"
                            :|style.left| 0
                            :|style.top| 0
                            :|style.overflow| "visible"
                            :|style.zIndex| 10
                            :|style.visibility| "hidden")))

      (setf (jscl::oget ctx1 "fillStyle") "black")
      ((jscl::oget ctx1 "fillText") ch (/ (- nx mw) 2) (- ny (/ (- ny mh) 2)))
      (let ((idat (jscl::oget ((jscl::oget ctx1 "getImageData") 0 0 nx ny) "data"))
            (odat (make-array (list nx ny))))
        (loop for i below nx do
          (loop for j below ny do
            (setf (aref odat (+ i (* nx j)))
                  (/ (aref idat (+ 3 (* 4 (+ i (* nx j))))) 255.0))))
        (setf (slot-value g 'matrix) odat)
        (add-plot g (normal-plot g))
        (append-element arrow (graph g))
        (append-element phi-div (graph g))
        (setf (jscl::oget (root g) "onmouseleave")
              (lambda (ev)
                (setf (jscl::oget arrow "style" "visibility") "hidden")
                (setf (jscl::oget phi-div "style" "visibility") "hidden")))
        (setf (jscl::oget (root g) "onmousemove")
              (lambda (ev)
                (setf (jscl::oget arrow "style" "visibility") "visible")
                (setf (jscl::oget phi-div "style" "visibility") "visible")
                (let* ((x (+ (xmin g)
                             (* (/ (- (jscl::oget ev "pageX") (jscl::oget (root g) "offsetLeft"))
                                   (jscl::oget (root g) "clientWidth"))
                                (- (xmax g) (xmin g)))))
                       (y (+ (ymin g)
                             (* (- 1
                                   (/ (- (jscl::oget ev "pageY") (jscl::oget (root g) "offsetTop"))
                                      (jscl::oget (root g) "clientHeight")))
                                (- (ymax g) (ymin g)))))
                       (angle (jsatan2 y x))
                       (phase (+ 0.75 (/ angle (* 2 pi))))
                       (phase (if (> phase 1) (- phase 1) phase)))
                  (setf (slot-value g 'cur-phase) phase)
                  (setf (jscl::oget arrow "style" "rotate")
                        (format nil "~Arad" (- (* 0.5 pi) angle)))
                  (setf (jscl::oget phi-div "innerHTML")
                        (subseq
                          (format nil "&#966;=~A000000000000" (* 0.001 (truncate (* 1000.0 phase))))
                          0 12)))))))))

(defun-f get-profile (m params phase spec-n snr ofs &optional (prog-cb (lambda (x) (declare (ignore x)))))
  (let* ((n (resolution params))
         (max-v (max-v params))
         (prof (make-instance 'profile :phase phase
                 :data (loop for i below spec-n collect
                         (cons (* 2.0 max-v (- (/ i (- spec-n 1)) 0.5)) nil))
                 :params params))
         (wm (weights prof prog-cb 0.1 0.9))
         (ii (/ 1.0 (loop for r in (data prof) and w across wm sum
                      (setf (cdr r)
                            (let ((x (loop for i below (length w) by 2 sum
                                       (* (aref m (aref w i)) (aref w (+ i 1))))))
                              x)))))
         (sa (* ii (/ 1.0 snr) (apply #'max (mapcar #'cdr (data prof))))))
    (loop for r in (data prof) do
      (setf (cdr r)
            (+ ofs (* 100.0 (+ (* sa (jsrandom)) (* ii (cdr r)))))))
    (funcall prog-cb 1.0)
    prof))

(defmethod-f get-a-div ((s sample-profile-source))
  (let* ((nx (resolution (params s)))
         (ny nx)
         (cnv1 (create-element "canvas" :|width| nx :|height| ny))
         (ctx1 ((jscl::oget cnv1 "getContext") "2d"))
         (xxx (setf (jscl::oget ctx1 "font") (format nil "~Apx serif" nx)))
         (m ((jscl::oget ctx1 "measureText") (sample-char s)))
         (mw (jscl::oget m "width"))
         (mh (jscl::oget m "actualBoundingBoxAscent")))
    (make-svg :|viewBox| "0 0 500 500"
              :|style.position| "absolute"
              :|style.width| "100%"
              :|style.height| "100%"
              :|style.left| "0"
              :|style.top| "0"
              :|style.zIndex| 18
              :|preserveAspectRatio| "none"
              :|style.font| (format nil "~Apx serif" nx)
              :|style.visibility| "hidden"
      `(text :|fill| "none"
             :|stroke| "red"
             :|stroke-width| "3px"
             :|stroke-linejoin| "round"
             :|style.fontSize| "500"
             :|x| ,(* 500 (/ (- nx mw) (* 2 nx)))
             :|y| ,(* 500 (- 1 (/ (- ny mh) (* 2 ny))))
             ,(sample-char s)))))

(defmethod-f get-controls ((s sample-profile-source) node &optional img matrix)
  (list
    (create-element "label"
      :append-element (create-element "span" :|style.marginRight| "1em"
                                             :|style.textDecorationStyle| "dashed"
                                             :|style.textDecorationLine| "underline"
                                             :|style.textDecorationThicknes| "1.75pt"
                                             :|style.color| "blue"
                                             :|innerHTML| "Show contour:")
      :append-element
        (let ((a-div nil)
              (cur-sym nil))
          (with-self box
            (create-element "input" :|type| "checkbox"
              :|onclick| (lambda (ev)
                           (when (not (equal cur-sym (sample-char s)))
                             (if a-div (remove-element a-div))
                             (setf a-div (get-a-div s))
                             (append-element a-div (parent-element node))
                             (ensure-element node
                               (setf (jscl::oget a-div "style" "fontSize")
                                     (jscl::oget node "clientHeight"))))
                           (setf (jscl::oget a-div "style" "visibility")
                                 (if (jscl::oget box "checked")
                                     "visible"
                                     "hidden")))))))))

(defmethod-f render-widget :after ((w get-prof-dialog))
  (append-element
    (create-element "div" :|style.border| "0.1em solid black"
                          :|style.border-radius| "0.5em"
                          :|style.padding| "0.5em"
                          :|style.background| "#fffff0"
      :append-element (create-element "div" :|style.width| "100%"
                                            :|style.paddingBottom| "1em"
                        :append-element "Making profile:")
      :append-element (render-widget (setf (slot-value w 'bar) (make-instance 'progress-bar)))
      :append-element (create-element "div" :|style.width| "100%"
                                            :|style.paddingTop| "1em"
                        :append-element
                          (create-element "button" :|innerHTML| "STOP"
                                                   :|onclick| (lambda (ev)
                                                                (funcall (stop-cb w) w)))))
    (root w)))

(defmethod-f render-widget :after ((w get-prof-progress))
  (append-element
    (create-element "div" :|style.border| "0.1em solid black"
                          :|style.border-radius| "0.5em"
                          :|style.padding| "0.5em"
                          :|style.background| "#fffff0"
      :append-element (create-element "div" :|style.width| "100%"
                                            :|style.paddingBottom| "1em"
                        :append-element "Making profiles:")
      :append-element (render-widget (setf (slot-value w 'bar) (make-instance 'progress-bar)))
      :append-element (create-element "div" :|style.width| "100%"
                                            :|style.paddingTop| "1em"
                        :append-element
                          (create-element "button" :|innerHTML| "STOP"
                                                   :|onclick| (lambda (ev)
                                                                (funcall (stop-cb w) w)))))
    (root w)))

(defmethod-f render-widget :after ((s sample-profile-source))
  (append-element
    (labels ((mkfld (name def fld &optional (is-int t) (cb (lambda (c) (declare (ignore c)))))
                (create-element "div" :|style.width| "100%"
                                      :|style.display| "inline-block"
                                      :|style.marginTop| "1em"
                  :append-element (create-element "span" :|style.marginRight| "0.5em"
                                     :append-element name)
                  :append-element
                    (create-element "span"
                      :append-element
                        (render-widget
                          (make-instance 'editable-field
                            :input-size 5
                            :value def
                              :ok (lambda (v)
                                    (if is-int
                                        (let ((v1 (js-parse-float v)))
                                          (if (> v1 0)
                                              (progn
                                                (setf (slot-value s fld) v1)
                                                (funcall cb v1)
                                                v1)))
                                        (progn
                                          (setf (slot-value s fld) v)
                                          (funcall cb v)
                                          v)))))))))

      (let* ((p (make-instance 'sample-picker
                 :xmin (- (max-v (params s))) :xmax (max-v (params s))
                 :ymin (- (max-v (params s))) :ymax (max-v (params s))
                 :scales '(:left :right :top :bottom)
                 :xcaption "V (km/s)" :ycaption (create-element "span" :|style.whiteSpace| "nowrap" :|innerHTML| "V (km/s)")
                 :sample-char (sample-char s)
                 :params (params s)))
             (a-div (get-a-div s)))
        (setf (slot-value s 'picker) p)
        (create-element "div" :|style.width| "20em"
                              :|style.textAlign| "center"
          :append-element
            (create-element "div" :|style.width| "90%"
                                  :|style.display| "inline-block"
              :append-element
                (let ((rp (render-widget p)))
                  (ensure-element rp
                    (setf (jscl::oget a-div "style" "fontSize")
                          (jscl::oget (graph p) "clientHeight"))
                    (append-element a-div (parent-element (graph p)))
                    (setf (jscl::oget (graph p) "onclick")
                          (lambda (ev)
                            (if (cur-phase p)
                                (let* ((pw (make-instance 'get-prof-dialog))
                                       (pcb (lambda (x) (set-progress (bar pw) x)))
                                       (np (npoints s))
                                       (snr (snr s))
                                       (ph (cur-phase p))
                                       (m (matrix p))
                                       (wrk (make-instance 'classic-worker :persistent-cache t))
                                       (n (resolution (params s)))
                                       (max-v (max-v (params s))))
                                  (register-main-lambda pcb)
                                  (append-element (render-widget pw))
                                  (setf (slot-value pw 'stop-cb)
                                        (lambda (x)
                                          (kill wrk)
                                          (close x)))
                                  (bind-exit-values-for (prof)
                                    (run-in-web-worker wrk
                                      (get-profile m (params s) ph np snr (offset s) pcb))
                                    (progn
                                      (add-profile s prof)
                                      (close pw))))))))
                  rp))
          :append-element (mkfld "Symbol:" (sample-char (picker s)) 'sample-char nil
                            (lambda (ch)
                              (remove-all-profiles s)
                              (redraw s)))
          :append-element
            (create-element "div" :|style.width| "100%"
                                  :|style.display| "inline-block"
                                  :|style.marginTop| "0.5em"
              :append-element
                (create-element "label"
                  :append-element (create-element "span" :|style.marginRight| "1em"
                                                         :|style.textDecorationStyle| "dashed"
                                                         :|style.textDecorationLine| "underline"
                                                         :|style.textDecorationThicknes| "1.75pt"
                                                         :|style.color| "blue"
                                                         :|innerHTML| "Show blured:")
                  :append-element
                    (with-self box
                      (create-element "input" :|type| "checkbox"
                        :|onclick| (lambda (ev)
                                     (if (jscl::oget box "checked")
                                         (progn
                                           (setf (jscl::oget (root (normal-plot (picker s))) "style" "visibility") "hidden")
                                           (if (slot-boundp (picker s) 'blured-plot)
                                               (setf (jscl::oget (root (blured-plot (picker s))) "style" "visibility") "visible")
                                               (add-plot (picker s) (blured-plot (picker s)))))
                                         (progn
                                           (setf (jscl::oget (root (normal-plot (picker s))) "style" "visibility") "visible")
                                           (if (slot-boundp (picker s) 'blured-plot)
                                               (setf (jscl::oget (root (blured-plot (picker s))) "style" "visibility") "hidden"))))
                                     t)))))
          :append-element
            (create-element "div" :|style.width| "100%"
                                  :|style.display| "inline-block"
                                  :|style.marginTop| "0.5em"
              :append-element
                (create-element "label"
                  :append-element (create-element "span" :|style.marginRight| "1em"
                                                         :|style.textDecorationStyle| "dashed"
                                                         :|style.textDecorationLine| "underline"
                                                         :|style.textDecorationThicknes| "1.75pt"
                                                         :|style.color| "blue"
                                                         :|innerHTML| "Show contour:")
                  :append-element
                    (with-self box
                      (create-element "input" :|type| "checkbox"
                        :|onclick| (lambda (ev)
                                     (setf (jscl::oget a-div "style" "visibility")
                                           (if (jscl::oget box "checked")
                                               "visible"
                                               "hidden"))
                                     t)))))
          :append-element (mkfld "Points in profile:" (npoints s) 'npoints)
          :append-element (mkfld "SNR:" (snr s) 'snr)
          :append-element
            (let ((inp (create-element "input" :|type| "text"
                                               :|value| "21"
                                               :|size| "5"
                                               :|style.marginLeft| "1em"
                                               :|style.marginRight| "1em"
                                               :|style.textAlign| "center")))
              (create-element "div" :|style.width| "100%"
                                    :|style.display| "inline-block"
                                    :|style.marginTop| "1em"
                :append-element "Take"
                :append-element inp
                :append-element
                  (create-element "button" :|style.width| "50%"
                                           :|innerHTML| "profiles"
                                           :|style.width| "10em"
                    :|onclick| (lambda (ev)
                                 (remove-all-profiles s)
                                 (let* ((n (floor (js-parse-float (jscl::oget inp "value"))))
                                        (d (/ 1.0 n))
                                        (ntasks (jscl::oget (winref "navigator") "hardwareConcurrency"))
                                        (workers (loop for i below ntasks collect (make-instance 'classic-worker :persistent-cache t)))
                                        (done-flags (loop for i below n collect nil))
                                        (pb (make-instance 'get-prof-progress
                                              :stop-cb (lambda (pb)
                                                         (map nil #'kill workers)
                                                         (close pb)))))
                                   (append-element (render-widget pb))
                                   (multiple-value-bind (x rem) (floor 0.5 d)
                                     (let ((p (picker s))
                                           (d (/ 1.0 (+ n (if (= 0 rem) 0.5 0)))))
                                       (loop for i below n do
                                         (multiple-value-bind (x nw) (floor i ntasks)
                                           (let* ((i i)
                                                  (ph (* i d))
                                                  (ww (nth nw workers))
                                                  (np (npoints s))
                                                  (snr (snr s))
                                                  (m (matrix p))
                                                  (n (resolution (params s)))
                                                  (max-v (max-v (params s))))
                                             (when-worker-free ww
                                               (bind-exit-values-for (prof)
                                                 (run-in-web-worker ww
                                                   (cache-vars ph)
                                                   (get-profile m (params s) ph np snr (offset s)))
                                                 (progn
                                                   (add-profile s prof)
                                                   (setf (nth i done-flags) t)
                                                   (set-progress (bar pb) (/ (count-if (complement #'null) done-flags)
                                                                             (length done-flags)))
                                                   (when (notany #'null done-flags)
                                                     (map nil #'kill workers)
                                                     (close pb)))))))))))))))
          :append-element
            (create-element "div" :|style.width| "100%"
                                  :|style.display| "inline-block"
                                  :|style.marginTop| "1em"
              :append-element
                (create-element "button" :|style.width| "50%"
                                         :|innerHTML| "clear all profiles"
                  :|onclick| (lambda (ev)
                               (remove-all-profiles s)))))))
    (src-root s)))
