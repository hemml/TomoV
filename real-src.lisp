(in-package :tomo)

(defclass-f real-profile-source (profile-source-widget)
  ((name :accessor name
         :initform nil)
   (files :accessor files)
   (primary-mass)
   (secondary-mass)
   (period)))

(defclass-f file-selector (omg-widget dont-transfer)
  ((items :accessor items
          :initform nil)
   (source :initarg :source
           :accessor source)
   (v-column :initform 1
             :initarg :v-column
             :accessor v-column)
   (i-column :initform 2
             :initarg :i-column
             :accessor i-column)))

(defclass-f file-selector-item (omg-widget)
  ((data :accessor data
         :initarg :data)
   (phase :initform nil
          :initarg :phase
          :accessor phase)
   (file :initarg :file
         :accessor file)
   (name :accessor name)
   (text :accessor text)
   (loaded :initform nil
           :accessor loaded)
   (parent :initarg :parent
           :accessor parent)
   (profile :accessor profile)
   (enabled :initform t
            :accessor enabled)))

(defclass-f nulable-fld (editable-field)
  ((value)
   (cancel :initform (lambda (w)
                       (if (or (equal (slot-value w 'value) "")
                               (equal (slot-value w 'value) 0))
                         (setf (slot-value w 'value) nil))
                       (if (not (slot-value w 'value))
                           (setf (jscl::oget (root w) "innerHTML") "not defined"))))))

(defmethod-f val ((e nulable-fld))
  (if (slot-value e 'value)
      (slot-value e 'value)
      ""))

(defmethod-f render-widget :after ((e nulable-fld))
  (if (not (slot-value e 'value))
      (setf (jscl::oget (root e) "innerHTML") "not defined")))


(defmethod-f update-params ((s real-profile-source) (p parameters))
  (remake-profiles s)
  (redraw s))

(lazy-slot primary-mass ((s real-profile-source))
  nil)

(lazy-slot secondary-mass ((s real-profile-source))
  nil)

(lazy-slot period ((s real-profile-source))
  nil)

(lazy-slot files ((s real-profile-source))
  (make-instance 'file-selector :source s))

(defmethod-f remake-profiles ((s real-profile-source))
  (remove-all-profiles s)
  (map nil #'remake-profile (items (files s))))

(defmethod-f source-loaded ((s real-profile-source)))
;;  (slot-makunbound (solver (solver s)) 'absorbtion-profile)
;;  (remake-profiles s))

(defmethod-f reload ((fi file-selector-item))
  (setf (slot-value fi 'loaded) nil)
  (let ((rd (jscl::make-new (winref "FileReader"))))
    (setf (jscl::oget rd "onload")
          (lambda (ev)
            (let* ((txt (jscl::oget rd "result"))
                   (parent (parent fi))
                   (lines ((jscl::oget (jscl::lisp-to-js txt) "split")
                           (jscl::make-new (winref "RegExp") (jscl::lisp-to-js (format nil "~C?~C" (code-char 13) (code-char 10))))))
                   (r1 (jscl::make-new (winref "RegExp") (jscl::lisp-to-js (format nil "[~C ]+" #\tab)))))
              (setf (slot-value fi 'text) txt)
              (setf (slot-value fi 'data)
                    (let ((ic (i-column parent))
                          (vc (v-column parent))
                          (st :hdr))
                      (loop for l across lines collect
                        (let* ((l1 (jscl::js-to-lisp l))
                               (flds ((jscl::oget l "split") r1))
                               (i (jscl::oget flds (1- ic)))
                               (i (if i (js-parse-float i)))
                               (v (jscl::oget flds (1- vc)))
                               (v (if v (js-parse-float v))))
                          (if (and (not (equal st :foot))
                                   i v
                                   (not (is-nan i))
                                   (not (is-nan v)))
                              (progn
                                (setf st :body)
                                (setf l1 (cons v i)))
                              (if (equal st :body) (setf st :foot)))
                          (cons st l1)))))
              (setf (slot-value fi 'loaded) t)
              (redraw fi))))
    ((jscl::oget rd "readAsText") (file fi))
    (redraw fi)))

(defmethod-f initialize-instance :after ((fi file-selector-item) &rest args)
  (setf (slot-value fi 'name) (jscl::oget (file fi) "name"))
  (reload fi))

(defmethod-f remove-file ((fi file-selector-item))
  (let ((parent (parent fi)))
    (setf (slot-value parent 'items) (remove-if (lambda (x) (eql x fi)) (items parent)))
    (if (slot-boundp fi 'profile) (remove-profile (source parent) (profile fi)))
    (redraw parent)))

(defmethod-f remake-profile ((fi file-selector-item))
  (let* ((loaded (loaded fi))
         (dat (if loaded (data fi)))
         (body (mapcar #'cdr (remove-if-not (lambda (x) (equal (car x) :body)) dat)))
         (phase (phase fi))
         (parent (parent fi))
         (max-v (max-v (params (source parent))))
         (ofs (offset (source parent))))
    (if (slot-boundp fi 'profile)
        (remove-profile (source parent) (profile fi)))
    (slot-makunbound fi 'profile)
    (if (and (enabled fi) phase)
        (add-profile (source parent)
          (setf (slot-value fi 'profile)
                (make-instance 'profile :phase phase
                    :data (loop for p in body when (and (>= (car p) (- max-v))
                                                        (<= (car p) max-v))
                            collect p)
                    :params (params (source parent))
                    :offset ofs))))))

(defmethod-f render-widget ((fi file-selector-item))
  (setf (slot-value fi 'root)
        (let* ((loaded (loaded fi))
               (dat (if loaded (data fi)))
               (body (mapcar #'cdr (remove-if-not (lambda (x) (equal (car x) :body)) dat)))
               (phase (phase fi))
               (parent (parent fi))
               (norm (and loaded (not (= (i-column parent) (v-column parent))) phase))
               (enabled (enabled fi))
               (clr1 "rgb(193, 253, 217)")
               (clr2 "rgb(255, 195, 195)")
               (clr3 "rgb(195, 195, 195)")
               (bgcol (if enabled (if norm clr1 clr2) clr3))
               (but (create-element "button" :|innerHTML| "remove"))
               (name (name fi))
               (tdl nil)
               (state phase))
          (labels ((upd (ev)
                     (remake-profile fi))
                   (rm (ev)
                     (remove-file fi))
                   (set-b-state (&optional init)
                     (setf (jscl::oget but "innerHTML") (if (and state (not init)) "update" "remove"))
                     (setf (jscl::oget but "onclick") (if (and state (not init)) #'upd #'rm))
                     (setf (jscl::oget but "style" "background") (if state clr1 clr2))
                     (loop for td in tdl do
                       (setf (jscl::oget td "style" "background") (if enabled (if state clr1 clr2) clr3))))
                   (make-td (&rest args)
                     (let ((td (apply #'create-element (cons "td" args))))
                       (push td tdl)
                       td)))
            (set-b-state t)
            (create-element "tr"
              :append-element
                (make-td :|innerHTML| name
                         :|style.padding| "0.25em 1em"
                         :|style.background| bgcol
                         :|style.textDecorationStyle| "dashed"
                         :|style.textDecorationLine| "underline"
                         :|style.textDecorationThicknes| "1.75pt"
                         :|style.color| "blue")
              :append-element
                (make-td :|innerHTML| (if loaded
                                          (format nil "~A" (length (remove-if-not (lambda (x) (equal (car x) :hdr)) dat)))
                                          "loading...")
                         :|style.padding| "0.25em 1em"
                         :|style.background| bgcol)
              :append-element
                (make-td :|innerHTML| (if loaded
                                          (format nil "~A" (length body))
                                          "loading...")
                         :|style.padding| "0.25em 1em"
                         :|style.background| bgcol)
              :append-element
                (make-td :|innerHTML| (if loaded
                                          (format nil "~A" (length (remove-if-not (lambda (x) (equal (car x) :foot)) dat)))
                                          "loading...")
                         :|style.padding| "0.25em 1em"
                         :|style.background| bgcol)
              :append-element
                (make-td :|innerHTML| (if loaded
                                          (format nil "~A ... ~A"
                                            ((jscl::oget (apply #'min (mapcar #'car body)) "toFixed") 3)
                                            ((jscl::oget (apply #'max (mapcar #'car body)) "toFixed") 3))
                                          "loading...")
                         :|style.padding| "0.25em 1em"
                         :|style.background| bgcol)
              :append-element
                (make-td :|innerHTML| (if loaded
                                          (format nil "~A ... ~A"
                                            ((jscl::oget (apply #'min (mapcar #'cdr body)) "toFixed") 3)
                                            ((jscl::oget (apply #'max (mapcar #'cdr body)) "toFixed") 3))
                                          "loading...")
                         :|style.padding| "0.25em 1em"
                         :|style.background| bgcol)
              :append-element
                (make-td :|style.padding| "0.25em 1em"
                         :|style.background| bgcol
                  :append-element
                    (with-self inp
                      (labels ((upd (ev)
                                 (let* ((val (jscl::oget inp "value"))
                                        (v (js-parse-float val)))
                                   (when (and (not (equal val "")) v (not (is-nan v)))
                                     (setf phase v)
                                     (setf (slot-value fi 'phase) v)
                                     (setf state t)
                                     (set-b-state)))
                                 t))
                        (create-element "input" :|type| "text"
                                                :|placeholder| "set phase"
                                                :|size| "7"
                                                :|style.marginLeft| "1em"
                                                :|style.marginRight| "1em"
                                                :|style.textAlign| "center"
                                                :|value| (if phase phase "")
                          :|onchange| #'upd
                          :|onkeyup| #'upd
                          :|onkeydown| #'upd
                          :|onpaste| #'upd
                          :|ondrop| #'upd))))
              :append-element
                (create-element "td"
                  :append-element
                    (with-self box
                      (create-element "input" :|type| "checkbox"
                                              :|checked| enabled
                        :|onclick| (lambda (ev)
                                     (let ((st (jscl::oget box "checked")))
                                       (setf enabled (setf (slot-value fi 'enabled) st))
                                       (remake-profile fi)
                                       (set-b-state)
                                       ; (redraw (trail (source (parent fi))))
                                       t)))))
              :append-element
                (create-element "td" :|style.padding| "0.25em 1em"
                  :append-element but))))))

(defmethod-f render-widget ((fs file-selector))
  (setf (slot-value fs 'root)
        (labels ((mk-sv (slot)
                   (lambda (val)
                         (let ((v1 (js-parse-float val)))
                           (when (and v1 (not (is-nan v1)) (= v1 (floor v1)))
                             (setf (slot-value fs slot) v1)
                             (loop for i in (items fs) do (reload i))
                             (redraw fs)
                             v1)))))
          (create-element "table" :|style.margin| "1em"
                                  :|style.borderSpacing| "0.2em"
            :append-element
              (create-element "tr"
                 :append-element (create-element "td" :|innerHTML| "File"
                                                      :|style.background| "rgb(255,232,201)"
                                                      :|style.padding| "0.25em 1em"
                                                      :|style.textAlign| "center")
                 :append-element (create-element "td" :|innerHTML| "header"
                                                      :|style.background| "rgb(255,232,201)"
                                                      :|style.padding| "0.25em 1em"
                                                      :|style.textAlign| "center")
                 :append-element (create-element "td" :|innerHTML| "points"
                                                      :|style.background| "rgb(255,232,201)"
                                                      :|style.padding| "0.25em 1em"
                                                      :|style.textAlign| "center")
                 :append-element (create-element "td" :|innerHTML| "footer"
                                                      :|style.background| "rgb(255,232,201)"
                                                      :|style.padding| "0.25em 1em"
                                                      :|style.textAlign| "center")
                 :append-element (create-element "td" :|style.background| "rgb(255,232,201)"
                                                      :|style.padding| "0.25em 1em"
                                                      :|style.textAlign| "center"
                                   :append-element "V (column "
                                   :append-element (render-widget (make-instance 'editable-field :value (v-column fs)
                                                                    :ok (mk-sv 'v-column)))
                                   :append-element ")")
                 :append-element (create-element "td" :|style.background| "rgb(255,232,201)"
                                                      :|style.padding| "0.25em 1em"
                                                      :|style.textAlign| "center"
                                   :append-element "I (column "
                                   :append-element (render-widget (make-instance 'editable-field :value (i-column fs)
                                                                    :ok (mk-sv 'i-column)))
                                   :append-element ")")
                 :append-element (create-element "td" :|innerHTML| "Phase"
                                                      :|style.background| "rgb(255,232,201)"
                                                      :|style.padding| "0.25em 1em"
                                                      :|style.textAlign| "center")
                 :append-element (create-element "td")
                 :append-element (create-element "td"))
            :append-elements (mapcar #'render-widget (items fs))))))

(defmethod-f render-widget :after ((s real-profile-source))
  (append-element
    (let ((fs (files s)))
      (create-element "div" :|style.textAlign| "center"
                            :|style.float| "left"
        :append-element
          (create-element "div"
            :append-element
              (create-element "span" :|innerHTML| "Continuum level:"
                                     :|style.marginRight| "1em")
            :append-element
              (render-widget (make-instance 'editable-field :value (offset s)
                               :ok (lambda (val)
                                     (let ((v (js-parse-float val)))
                                       (when (and v (not (is-nan v)))
                                         (setf (slot-value s 'offset) v)
                                         (remake-profiles s)
                                         v))))))
        :append-element
          (create-element "div" :|style.marginTop| "1em"
            :append-element
              (create-element "span" :|innerHTML| "Primary mass (solar massess):"
                                     :|style.marginRight| "1em")
            :append-element
              (render-widget (make-instance 'nulable-fld :value (primary-mass s)
                               :ok (lambda (val)
                                     (let ((v (js-parse-float val)))
                                       (when (and v (not (is-nan v)) (> v 0))
                                         (setf (slot-value s 'primary-mass) v)))))))
        :append-element
          (create-element "div" :|style.marginTop| "1em"
            :append-element
              (create-element "span" :|innerHTML| "Secondary mass (solar massess):"
                                     :|style.marginRight| "1em")
            :append-element
              (render-widget (make-instance 'nulable-fld :value (secondary-mass s)
                               :ok (lambda (val)
                                     (let ((v (js-parse-float val)))
                                       (when (and v (not (is-nan v)) (> v 0))
                                         (setf (slot-value s 'secondary-mass) v)))))))
        :append-element
          (create-element "div" :|style.marginTop| "1em"
            :append-element
              (create-element "span" :|innerHTML| "Orbital period (days):"
                                     :|style.marginRight| "1em")
            :append-element
              (render-widget (make-instance 'nulable-fld :value (period s)
                               :ok (lambda (val)
                                     (let ((v (js-parse-float val)))
                                       (when (and v (not (is-nan v)) (> v 0))
                                         (setf (slot-value s 'period) v)))))))

        :append-element
          (create-element "div" :|style.width| "100%"
                                :|style.display| "inline-block"
                                :|style.marginTop| "1em"
            :append-element
              (create-element "button" :|style.width| "50%"
                                       :|innerHTML| "remake profiles"
                :|onclick| (lambda (ev)
                             (remake-profiles s))))
        :append-element
          (create-element "div" :|style.width| "100%"
                                :|style.display| "inline-block"
                                :|style.marginTop| "1em"
            :append-element "Add a profile(s):"
            :append-element
              (with-self fil
                (create-element "input" :|type| "file"
                                        :|multiple| "1"
                                        :|style.width| "50%"
                                        :|style.marginLeft| "2em"
                  :|onchange| (lambda (ev)
                                (setf (slot-value fs 'items)
                                      (concatenate 'list
                                        (loop for f across (jscl::oget fil "files")
                                          collect (make-instance 'file-selector-item
                                                                 :file f
                                                                 :parent fs))
                                        (items fs)))
                                (redraw fs)))))
        :append-element (render-widget fs)))
    (src-root s)))

(defun-f roche-pot (x y q)
  (let ((q1 (1+ q)))
    (- (+ (/ 2 (* q1 (sqrt (+ (sqr x) (sqr y)))))
          (/ (* 2 q)
             (* q1 (sqrt (+ (sqr (- 1 x)) (sqr y)))))
          (sqr (- x (/ q q1)))
          (sqr y)))))

(defun-f roche-force (x y q)
  (values (+ (/ (* -1 x) (expt (+ (sqr x) (sqr y)) 1.5))
             (/ (* q -1 (- x 1)) (expt (+ (sqr (- x 1)) (sqr y)) 1.5))
             x
             (- q))
          (+ (/ (* -1 y) (expt (+ (sqr x) (sqr y)) 1.5))
             (/ (* q -1 y) (expt (+ (sqr (- x 1)) (sqr y)) 1.5))
             y)))

(defun-f find-lp (x1 x2 q)
  (loop do
    (let ((x3 (* 0.5 (+ x1 x2))))
      (if (< (roche-force x3 0 q) 0)
          (setf x1 x3)
          (setf x2 x3))
      (when (< (- x2 x1) 1e-5)
        (return-from find-lp (values x3 (roche-pot x3 0 q)))))))0

(defun-f get-roche-lobe (q &optional (x0 0) (y0 0))
  (multiple-value-bind (xl1 l1pot) (find-lp 0 1 q)
    (let* ((xl2 (find-lp (if (> x0 0) 1 -5)
                         (if (> x0 0) 5 0)
                         q))
           (rp1 (abs (- x0 xl1)))
           (rp2 (abs (- x0 xl2))))
      (labels ((find-pot (phi r1 r2)
                 (let ((r3 (* 0.5 (+ r1 r2))))
                   (if (< (- r2 r1) 1e-6)
                       (values (+ x0 (* r3 (jscos phi)))
                               (+ y0 (* r3 (jssin phi))))
                       (if (> l1pot
                              (roche-pot (+ x0 (* r3 (jscos phi)))
                                         (+ y0 (* r3 (jssin phi)))
                                         q))
                           (find-pot phi r3 r2)
                           (find-pot phi r1 r3))))))
        (let ((phi (* pi -0.999)))
          (loop while (< phi (* 0.999 pi)) collect
            (prog1
              (apply #'cons (multiple-value-list (find-pot phi 0 (if (> (roche-pot (+ x0 (* rp2 (jscos phi)))
                                                                                   (+ y0 (* rp2 (jssin phi)))
                                                                                   q)
                                                                        l1pot)
                                                                     rp2
                                                                     rp1))))
              (incf phi (* 0.005 pi)))))))))

(defclass-f roche-plot (plot)
  ((q :initarg :qprop
      :accessor qprop)))

; (defclass-f l1-stream-plot (plot)
;   ((q :initarg :qprop
;       :accessor qprop)
;    (v0 :initarg :v0
;        :accessor v0)))

(defclass-f roche-v-plot (plot)
  ((q :initarg :qprop
      :accessor qprop)
   (aomega :initarg :aomega
           :accessor aomega)))

(defclass-f disk-v-plot (plot)
  ((q :initarg :qprop
      :accessor qprop)
   (aomega :initarg :aomega
           :accessor aomega)
   (primary :initarg :primary
            :accessor primary)
   (fill :initform nil
         :accessor fill)))

(defclass-f disk-plot (plot)
  ((q :initarg :qprop
      :accessor qprop)
   (aomega :initarg :aomega
           :accessor aomega)
   (primary :initarg :primary
            :accessor primary)
   (fill :initform nil
         :accessor fill)
   (x0 :accessor x0)
   (rd :accessor rd)))

(defmethod-f initialize-instance :after ((d disk-plot) &rest args)
  (setf (slot-value d 'x0) (if (primary d) 0 1))
  (setf (slot-value d 'rd) (let ((q (if (primary d) (qprop d) (/ 1 (qprop d)))))
                             (/ 0.6 (1+ q)))))

(defmethod-f make-curve ((p roche-plot))
  `(path :|stroke| ,(color p)
         :|stroke-width| "1px"
         :|vector-effect| "non-scaling-stroke"
         :|fill| "none"
         :|d| ,(apply #'jscl::concat
                      (labels ((mklob (lb)
                                 (cons (format nil "M~A,~A " (caar lb) (cdar lb))
                                       (mapcar (lambda (p)
                                                 (format nil "L~A,~A " (car p) (cdr p)))
                                               (cdr lb)))))
                        (append (mklob (get-roche-lobe (qprop p) 0.0))
                                (mklob (get-roche-lobe (qprop p) 1.0)))))))

; (defmethod-f make-curve ((p l1-stream-plot))
;   `(path :|stroke| ,(color p)
;          :|stroke-width| "1px"
;          :|vector-effect| "non-scaling-stroke"
;          :|fill| "none"
;          :|d| ,(apply #'jscl::concat
;                       (let ((x (find-lp 0 1 (qprop p)))
;                             (y 0)
;                             (vx (* 1e-6 (vp p)))
;                             (vy 0))))))


(defmethod-f make-curve ((p roche-v-plot))
  (let ((q (qprop p))
        (aomega (aomega p)))
    `(path :|stroke| ,(color p)
           :|stroke-width| "1px"
           :|vector-effect| "non-scaling-stroke"
           :|fill| "none"
           :|d| ,(apply #'jscl::concat
                        (labels ((mklob (lb)
                                   (cons (format nil "M~A,~A " (caar lb) (cdar lb))
                                         (mapcar (lambda (p)
                                                   (format nil "L~A,~A " (car p) (cdr p)))
                                                 (cdr lb))))
                                 (x-to-v (xy)
                                   (let* ((xmc (/ q (1+ q)))
                                          (x (- (car xy) xmc))
                                          (y (cdr xy)))
                                     (cons (* aomega y) (* -1 aomega x)))))
                          (append (mklob (mapcar #'x-to-v (get-roche-lobe q 0.0)))
                                  (mklob (mapcar #'x-to-v (get-roche-lobe q 1.0)))))))))

(defmethod-f make-curve ((p disk-v-plot))
  (let ((q (qprop p))
        (aomega (aomega p)))
    `(path :|stroke| ,(color p)
           :|stroke-width| "1px"
           :|vector-effect| "non-scaling-stroke"
           :|fill| ,(if (fill p) "blue" "none")
           :|fill-opacity| 0.05
           :|d| ,(let ((r (* (aomega p) (/ 1 (sqrt 0.6))))
                       (y (* (aomega p) (/ (if (primary p) (qprop p) -1) (1+ (qprop p)))))
                       (vmax (* 2.0 (xmax (parent p)))))
                   (format nil "M 0,~A A ~A ~A 0 1 0 0,~A A ~A ~A 0 1 0 0,~A z M ~A,~A h ~A v ~A h ~A v ~A z"
                               (+ y r)
                               r r (- y r)
                               r r (+ y r)
                               (- vmax) (- vmax) (* 2 vmax) (* 2 vmax) (- (* 2 vmax)) (- (* 2 vmax)))))))

(defmethod-f make-curve ((p disk-plot))
  (let ((q (qprop p))
        (aomega (aomega p)))
    `(circle :|stroke| ,(color p)
             :|stroke-width| "1px"
             :|vector-effect| "non-scaling-stroke"
             :|fill| ,(if (fill p) "blue" "none")
             :|fill-opacity| 0.05
             :|cy| 0
             :|cx| ,(x0 p)
             :|r| ,(rd p))))

(defparameter-f *roche-grf* nil)

(defun-f plot-roche (&optional (q 1.0))
  (if *roche-grf* (remove-element *roche-grf*))
  (append-element
    (setf *roche-grf*
          (create-element "div" :|style.width| "500px"
                                :|style.height| "500px"
            :append-element (let ((g (make-instance 'graph :xmin -1 :xmax 2 :ymin -1.5 :ymax 1.5)))
                              (add-plot g (make-instance 'roche-plot :qprop q))
                              (render-widget g))))))

(defmethod-f aomega ((s real-profile-source))
  (* 212.9 (expt (/ (+ (secondary-mass s) (primary-mass s))
                    (period s))
                 (/ 1 3))))

(defmethod-f qprop ((s real-profile-source))
  (/ (secondary-mass s) (primary-mass s)))

(defmethod-f get-controls ((s real-profile-source) node &optional img)
  (macrolet ((make-label (name &rest code)
               `(with-self div
                  (create-element "div"
                    :append-element
                      (create-element "label"
                        :append-element (create-element "span" :|style.marginRight| "1em"
                                                               :|style.textDecorationStyle| "dashed"
                                                               :|style.textDecorationLine| "underline"
                                                               :|style.textDecorationThicknes| "1.75pt"
                                                               :|style.color| "blue"
                                                               :|innerHTML| ,name)`
                        :append-element
                          (with-self box
                            (create-element "input" :|type| "checkbox"
                              :|onclick| (lambda (ev)
                                           (let ((checked (jscl::oget box "checked")))
                                             ,@code)
                                           t)))))))
             (make-plot-label (name element toggle inst)
               `(make-label ,name
                  (,toggle checked)
                  (if ,element (remove-plot ,element))
                  (setf ,element nil)
                  (when checked
                    (setf ,element ,inst)
                    (add-plot img ,element))))
             (toggle-disk (pr el)
               `(when picker-grf
                  (if show
                      (add-plot picker-grf (setf ,el (make-instance 'disk-plot :qprop (qprop s) :aomega (aomega s) :primary ,pr :color "blue")))
                      (when ,el
                        (remove-plot ,el)
                        (setf ,el nil))))))
    (let ((picker-div nil)
          (picker-grf nil)
          (lobes-plot nil)
          (primary-disk-plot nil)
          (secondary-disk-plot nil)
          (primary-disk-v-plot nil)
          (secondary-disk-v-plot nil)
          (roche-v-plot nil))
      (labels ((toggle-lobes (show)
                 (when picker-grf
                   (if show
                       (add-plot picker-grf (setf lobes-plot (make-instance 'roche-plot :qprop (qprop s))))
                       (when lobes-plot
                         (remove-plot lobes-plot)
                         (setf lobes-plot nil)))))
               (toggle-primary-disk (show)
                 (toggle-disk t primary-disk-plot))
               (toggle-secondary-disk (show)
                 (toggle-disk nil secondary-disk-plot)))
        (list
          (make-label "Show picker:"
            (if picker-div (remove-element picker-div))
            (setf picker-div nil)
            (setf picker-grf nil)
            (when checked
              ((jscl::oget (parent-element div) "insertBefore")
               (setf picker-div
                     (create-element "div" :|style.width| "100%"
                                           :|style.textAlign| "center"
                       :append-element
                         (create-element "span" :|style.width| "80%"
                                                :|style.display| "inline-block"
                           :append-element
                             (prog1
                               (render-widget
                                 (setf picker-grf
                                       (make-instance 'graph :xmin -1 :xmax 2 :ymin -1 :ymax 1 :preserve-aspect-ratio t)))
                               (let* ((grf (graph picker-grf))
                                      (igrf (create-element "div" :|style.position| "absolute"
                                                                  :|style.width| "20px"
                                                                  :|style.height| "20px"
                                                                  :|style.transform| "translate(-10px,-10px)"
                                              :append-element
                                                (make-svg :|viewBox| "0 0 100 100"
                                                          :|style.width| "100%"
                                                          :|style.height| "100%"
                                                          :|style.left| "0"
                                                          :|style.top| "0"
                                                          :|style.display| "block"
                                                          :|style.position| "absolute"
                                                          :|preserveAspectRatio| "none"
                                                  '(g :|stroke| "magenta"
                                                      :|vector-effect| "non-scaling-stroke"
                                                      :|fill| "none"
                                                      (path :|stroke-width| "0.2em" :|d| "M 0,50 h 39 M 50,0 v 39 M 100,50 h -39 M 50,100 v -39")
                                                      (circle :|stroke-width| "0.2em" :|cx| 50 :|cy| 50 :|r| 30)))))
                                      (q (qprop s))
                                      (ao (aomega s))
                                      (omega (/ (* 2 pi) (period s)))
                                      (vmax (max-v (params s)))
                                      (cf (/ 100 ;(jscl::oget (graph img) "clientWidth")
                                             (* 2 vmax))))
                                 (append-element igrf (graph img))
                                 (setf (jscl::oget grf "onmousemove")
                                       (lambda (ev)
                                         (oget-bind (x y) ev ("layerX" "layerY")
                                           (oget-bind (wi he) grf ("clientWidth" "clientHeight")
                                             (destructuring-bind (x y) (mapcar (lambda (x w mi ma)
                                                                                 (+ mi (* (- ma mi) (/ x w))))
                                                                               (list x (- he y))
                                                                               (list wi he)
                                                                               (list (xmin picker-grf) (ymin picker-grf))
                                                                               (list (xmax picker-grf) (ymax picker-grf)))
                                               (let ((i-vx 0)
                                                     (i-vy 0))
                                                 (map nil
                                                      (lambda (p vp)
                                                        (if p
                                                            (let* ((r (sqrt (+ (sqr (- x (x0 p))) (sqr y))))
                                                                   (in (< r (rd p))))
                                                              (when (or (and in (not (fill p)))
                                                                        (and (not in) (fill p)))
                                                                (setf (slot-value vp 'fill) (setf (slot-value p 'fill) (not (fill p))))
                                                                (remove-plot p)
                                                                (remove-plot vp)
                                                                (add-plot picker-grf p)
                                                                (add-plot img vp))
                                                              (when in
                                                                (let ((vk (* ao (sqrt (/ (if (primary p) 1 q)
                                                                                         (* r (1+ q)))))))

                                                                  (setf i-vx (/ (- (* vk y)) r))
                                                                  (setf i-vy (/ (* vk (- x (x0 p))) r))
                                                                  (setf x (x0 p))
                                                                  (setf y 0))))))
                                                      (list primary-disk-plot secondary-disk-plot)
                                                      (list primary-disk-v-plot secondary-disk-v-plot))
                                                 (let* ((x (- x (/ q (1+ q))))
                                                        (vx (+ (* ao y) i-vx))
                                                        (vy (+ (* ao x) i-vy)))
                                                   (setf (jscl::oget igrf "style" "left") (format nil "~A%" (* cf (+ vx vmax))))
                                                   (setf (jscl::oget igrf "style" "top") (format nil "~A%" (* cf (- vmax vy))))))))))))))))
               div)))
          (make-plot-label "Show Roche lobes:" roche-v-plot toggle-lobes (make-instance 'roche-v-plot :qprop (qprop s) :aomega (aomega s)))
          (make-plot-label "Show primary disk:" primary-disk-v-plot toggle-primary-disk (make-instance 'disk-v-plot :qprop (qprop s) :aomega (aomega s) :primary t :color "blue"))
          (make-plot-label "Show secondary disk:" secondary-disk-v-plot toggle-secondary-disk (make-instance 'disk-v-plot :qprop (qprop s) :aomega (aomega s) :primary nil :color "blue")))))))
