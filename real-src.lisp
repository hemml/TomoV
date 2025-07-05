(in-package :tomo)

(defclass-conf real-profile-source (profile-source-widget)
  ((name :accessor name
         :initform nil)
   (files :accessor files)
   (primary-mass :initform 1.0
                 :desc "Primary mass (solar masses)"
                 :type :number
                 :validator (lambda (v) (> v 0)))
   (secondary-mass :initform 1.0
                   :desc "Secondary mass (solar masses)"
                   :type :number
                   :validator (lambda (v) (> v 0)))
   (period :initform 1.0
           :desc "Orbital period (days)"
           :type :number
           :validator (lambda (v) (> v 0)))))


(lazy-slot primary-mass ((s real-profile-source))
  1.0)

(lazy-slot secondary-mass ((s real-profile-source))
  1.0)

(lazy-slot period ((s real-profile-source))
  1.0)


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
                    :params (params (source parent))))))))

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
        :append-elements (render-config s)
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
        :append-element
          (create-element "div" :|style.width| "100%"
                                :|style.display| "inline-block"
                                :|style.marginTop| "1em"
            :append-element "Load phases from file:"
            :append-element
              (with-self fil
                (create-element "input" :|type| "file"
                                        :|style.width| "50%"
                                        :|style.marginLeft| "2em"
                  :|onchange| (lambda (ev)
                                (let ((rd (jscl::make-new (winref "FileReader"))))
                                  (setf (jscl::oget rd "onload")
                                        (lambda (ev)
                                          (let* ((lines ((jscl::oget (jscl::lisp-to-js (jscl::oget rd "result")) "split")
                                                         (jscl::make-new (winref "RegExp")
                                                                         (jscl::lisp-to-js
                                                                           (format nil "~C?~C" (code-char 13) (code-char 10))))))
                                                 (r1 (jscl::make-new (winref "RegExp") (jscl::lisp-to-js (format nil "[~C ]+" #\tab)))))
                                            (loop for l across lines do
                                              (let* ((flds ((jscl::oget (jscl::lisp-to-js l) "split") r1))
                                                     (phase1 (ignore-errors (js-parse-float (jscl::oget flds 0))))
                                                     (phase2 (ignore-errors (js-parse-float (jscl::oget flds 1))))
                                                     (itm1 (ignore-errors (find (jscl::oget flds 0) (items fs) :key #'name :test #'equal)))
                                                     (itm2 (ignore-errors (find (jscl::oget flds 1) (items fs) :key #'name :test #'equal))))
                                                (jslog flds phase1 phase2 itm1 itm2)
                                                (destructuring-bind (item phase)
                                                  (cond ((and itm2 (numberp phase1) (not (is-nan phase1))) (list itm2 phase1))
                                                        ((and itm1 (numberp phase2) (not (is-nan phase2))) (list itm1 phase2))
                                                        (t (list nil nil)))
                                                  (when item
                                                    (setf (slot-value item 'phase) phase)))))
                                            (redraw fs)
                                            (remake-profiles s))))
                                  ((jscl::oget rd "readAsText") (jscl::oget fil "files" 0)))))))

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
  (values (- (+ (/ x (expt (+ (sqr x) (sqr y)) 1.5))
                (/ (* q (- x 1)) (expt (+ (sqr (- x 1)) (sqr y)) 1.5))
                (- (* x (1+ q)))
                q))
          (- (+ (/ y (expt (+ (sqr x) (sqr y)) 1.5))
                (/ (* q y) (expt (+ (sqr (- x 1)) (sqr y)) 1.5))
                (- (* y (1+ q)))))))

(defun-f find-lp (x1 x2 q)
  (loop do
    (let ((x3 (* 0.5 (+ x1 x2))))
      (if (< (roche-force x3 0 q) 0)
          (setf x1 x3)
          (setf x2 x3))
      (when (< (- x2 x1) 1e-5)
        (return-from find-lp (values x3 (roche-pot x3 0 q)))))))0

(defun-f get-roche-lobe (q x0)
  (multiple-value-bind (xl1 l1pot) (find-lp 0 1 q)
    (let ((max-r (* 0.99 (abs (- (find-lp (if (> x0 0) 0 -5) (if (> x0 0) 5 1) q) x0))))
          (min-r (* 0.5 (abs (- xl1 x0)))))
      (loop for i from 0 below 100 collect
        (let* ((phi (* 2 pi (+ (if (> x0 0) 0.5 0) (/ i 100))))
               (sin (jssin phi))
               (cos (jscos phi)))
          (labels ((get-rf (r)
                     (let ((x (* r cos))
                           (y (* r sin)))
                       (multiple-value-bind (fx fy) (roche-force (+ x x0) y q)
                         (+ (* fx x) (* fy y))))))
            (let* ((r1 min-r)
                   (r2 max-r)
                   (n 0))
              (loop while (and (< n 100) (> (- r2 r1) 1e-6)) do
                (let* ((r3 (* 0.5 (+ r1 r2))))
                  (incf n)
                  (if (and (< (roche-pot (+ x0 (* r3 cos)) (* r3 sin) q) l1pot)
                           (< (get-rf r3) 0))
                      (setf r1 r3)
                      (setf r2 r3))))
              (let ((r (* 0.5 (+ r1 r2))))
                (cons (+ x0 (* r cos)) (* r sin))))))))))

(defclass-f mappable-object ()
  ((source :initarg :source
           :accessor source)
   (matrix :accessor matrix
           :initarg :matrix)
   (v-map-res :accessor v-map-res
              :initform 200
              :initarg :v-map-res)
   (xy-plot :accessor xy-plot)
   (v-plot :accessor v-plot)
   (mmax :accessor mmax
         :initform nil)
   (xy-cnv)
   (v-cnv)
   (v-map)
   (name :accessor name)
   (active :accessor active
           :initform nil)
   (selected :accessor selected
             :initform nil)
   (highlited :accessor highlited
              :initform nil)
   (contour)
   (bounds)
   (zindex :initform 0
           :accessor zindex
           :initarg :zindex)))

(defclass-f mappable-plot (plot)
  ((source :initarg :source
           :accessor source)
   (space :initarg :space
          :accessor space)))

(defmethod-f render-widget ((p mappable-plot))
  (let ((root (call-next-method)))
    (when (equal (space p) :xy)
      (setf (jscl::oget root "style" "zIndex") (zindex (source p))))
    root))

(lazy-slot v-map ((mo mappable-object))
  (let* ((resolution (resolution (params (source mo))))
         (v-map-res (v-map-res mo))
         (arr (make-array (list (sqr resolution))))
         (vmax (max-v (params (source mo))))
         (x2v (xy-to-v mo)))
    (destructuring-bind (xmin xmax ymin ymax) (bounds mo)
      (labels ((v2i (v) (floor (* resolution (* 0.5 (1+ (/ v vmax))))))
               (i2x (i mi ma) (+ mi (* (- ma mi) (/ i v-map-res)))))
        (loop for i below v-map-res do
          (loop for j below v-map-res do
            (let* ((idx (+ i (* v-map-res j)))
                   (x (i2x i xmin xmax))
                   (y (i2x j ymax ymin))
                   (v (funcall x2v (cons x y))))
              (when v
                (let* ((vi (v2i (car v)))
                       (vj (- resolution 1 (v2i (cdr v)))))
                  (when (and (>= vi 0) (< vi resolution)
                             (>= vj 0) (< vj resolution))
                    (let ((v-idx (+ vi (* resolution vj))))
                      (setf (aref arr v-idx)
                            (cons idx (aref arr v-idx))))))))))))
    arr))

(defmethod-f map-intensity ((mo mappable-object) &optional indexes)
  (let* ((xy-cnv (xy-cnv mo))
         (xy-ctx ((jscl::oget xy-cnv "getContext") "2d"))
         (vmres (v-map-res mo))
         (xy-idat ((jscl::oget xy-ctx "getImageData") 0 0 vmres vmres))
         (xy-dat (jscl::oget xy-idat "data"))
         (matrix (matrix mo))
         (vmap (v-map mo))
         (resolution (resolution (params (source mo)))))
    (destructuring-bind (&optional mmax last-mmax) (mmax mo)
      (let* ((mmax (if (and mmax last-mmax (< (- (now) last-mmax) 1))
                       mmax
                       (let ((mmax (apply #'max (loop for i below (sqr resolution) collect (aref matrix i)))))
                         (car (setf (slot-value mo 'mmax) (list mmax (now))))))))
        (labels ((mp (idx)
                  (let ((pts (aref vmap idx)))
                    (when pts
                      (let ((ii (/ (aref matrix idx) (* mmax)))) ; (length pts)))))
                        (map nil (lambda (idx2)
                                    (let* ((oi (aref xy-dat (+ 0 (* 4 idx2))))
                                           (oi (if (> oi 0) oi 255)))
                                       (setf (aref xy-dat (+ 0 (* 4 idx2)))
                                             (setf (aref xy-dat (+ 1 (* 4 idx2)))
                                                   (setf (aref xy-dat (+ 2 (* 4 idx2)))
                                                         (min oi (* 255 (- 1 ii))))))
                                       (setf (aref xy-dat (+ 3 (* 4 idx2))) 255)))
                                 pts))))))
          (if indexes
              (map nil #'mp indexes)
              (let* ((v-cnv (v-cnv mo))
                     (v-ctx ((jscl::oget v-cnv "getContext") "2d"))
                     (v-idat ((jscl::oget v-ctx "getImageData") 0 0 resolution resolution))
                     (v-dat (jscl::oget v-idat "data")))
                (loop for i below (sqr resolution)
                  when (> (aref v-dat (+ 3 (* i 4))) 0) do (mp i)))))
        ((jscl::oget xy-ctx "putImageData") xy-idat 0 0)))))

(lazy-slot v-cnv ((mo mappable-object))
  (let ((resolution (resolution (params (source mo)))))
    (labels ((make-ev (btn)
               (lambda (ev)
                 (when (and (or (not btn) (> (jscl::oget ev "buttons") 0)))
                   (let* ((cnv (jscl::oget ev "target"))
                          (ctx ((jscl::oget cnv "getContext") "2d"))
                          (idat ((jscl::oget ctx "getImageData") 0 0 resolution resolution))
                          (dat (jscl::oget idat "data")))
                     (oget-bind (x y) ev ("layerX" "layerY")
                       (oget-bind (wi he) cnv ("clientWidth" "clientHeight")
                         (let* ((i0 (floor (* resolution x) wi))
                                (j0 (floor (* resolution y) he))
                                (delta (max 1 (floor resolution 50)))
                                (indexes (loop for i from (- i0 delta) below (+ 1 i0 delta) append
                                           (remove-if #'null
                                             (loop for j from (- j0 delta) below (+ 1 j0 delta) collect
                                               (when (and (>= i 0)
                                                          (>= j 0)
                                                          (< i resolution)
                                                          (< j resolution)
                                                          (<= (+ (sqr (- i0 i)) (sqr (- j0 j)))
                                                              (sqr delta)))
                                                 (let* ((idx (+ i (* resolution j)))
                                                        (idx4 (* 4 idx)))
                                                   (when (= 0 (aref dat (+ idx4 3)))
                                                     (setf (aref dat (+ idx4 2)) 255)
                                                     (setf (aref dat (+ idx4 3)) 100)
                                                     idx))))))))
                           (if indexes (map-intensity mo indexes))
                           ((jscl::oget ctx "putImageData") idat 0 0)
                           (redraw (xy-plot mo))))))))))
      (create-element "canvas" :|width|  resolution
                               :|height| resolution
                               :|style.width| "100%"
                               :|style.height| "100%"
                               :|style.left| "0"
                               :|style.top| "0"
                               :|style.position| "absolute"
                               :|style.zIndex| 10
        :|onclick| (make-ev nil)
        :|onmousemove| (make-ev t)))))

(lazy-slot xy-cnv ((mo mappable-object))
  (create-element "canvas" :|width| (v-map-res mo)
                           :|height| (v-map-res mo)))

(defmethod-f make-curve ((p mappable-plot))
  (let ((yofs (+ (ymax (parent p)) (ymin (parent p))))
        (clip-name (omgui::random-id)))
    (destructuring-bind (xmin xmax ymin ymax) (bounds (source p))
      (let ((path (funcall (if (equal :xy (space p)) #'get-xy-path #'get-v-path)
                           (source p)
                           :yofs yofs)))
        `(g ,@(if (equal :xy (space p))
                  `((defs (:|clipPath| :|id| ,clip-name
                             (path :|fill| "none"
                                   ,@path)))
                    (image :|clip-path| ,(format nil "url(#~A)" clip-name)
                           :|width| ,(- xmax xmin)
                           :|height| ,(- ymax ymin)
                           :|preserveAspectRatio| "none"
                           :|x| ,xmin
                           :|y| ,(- yofs ymax)
                           :|href| ,((jscl::oget (xy-cnv (source p)) "toDataURL") "image/png")
                           :|style.zIndex| ,(1- (zindex (source p))))))
            (path :|stroke| ,(color p)
                  :|stroke-width| "1px"
                  :|vector-effect| "non-scaling-stroke"
                  :|fill| ,(if (highlited (source p)) "rgba(0,0,255,0.05)" "none")
                  :|style.zIndex| ,(zindex (source p))
                  ,@path))))))

(lazy-slot bounds ((p mappable-object))
  (let ((contour (contour p)))
    (let ((xl (mapcar #'car contour))
          (yl (mapcar #'cdr contour)))
      (list (apply #'min xl) (apply #'max xl)
            (apply #'min yl) (apply #'max yl)))))

(lazy-slot xy-plot ((mo mappable-object))
  (make-instance 'mappable-plot :source mo :space :xy))

(lazy-slot v-plot ((mo mappable-object))
  (make-instance 'mappable-plot :source mo :space :v))

(defmethod-f inside ((mo mappable-object) x y)
  (let ((cont (contour mo)))
    (destructuring-bind (xmin xmax ymin ymax) (bounds mo)
      (and (>= x xmin)
           (<= x xmax)
           (>= y ymin)
           (<= y ymax)
           (oddp (loop for p1 in cont and p2 in (cdr cont) sum
                   (let* ((dx (- (car p2) (car p1)))
                          (k (if (/= dx 0) (/ (- (cdr p2) (cdr p1)) dx))))
                     (if (and (>= y (min (cdr p1) (cdr p2)))
                              (< y (max (cdr p1) (cdr p2)))
                              (or (not k)
                                  (>= (+ (/ (- y (cdr p1)) k)
                                         (car p1))
                                      x)))
                         1
                         0))))))))

(defmethod-f get-path ((mo mappable-object) &key space yofs)
  (let ((lb (mapcar (if (equal space :xy) #'identity (xy-to-v mo)) (contour mo))))
    (apply #'jscl::concat
      `(,(format nil "M ~A,~A " (caar lb) (- yofs (cdar lb)))
        ,@(mapcar (lambda (p) (format nil "L~A,~A " (car p) (- yofs (cdr p)))) (cdr lb))
        "Z"))))

(defmethod-f get-xy-path ((mo mappable-object) &key yofs)
  `(:|d| ,(get-path mo :space :xy :yofs yofs)))

(defmethod-f get-v-path ((mo mappable-object) &key yofs)
  `(:|d| ,(get-path mo :space :v :yofs yofs)))

(defmethod-f xy-to-v ((mo mappable-object))
  nil)


(defmethod-f setup-elements ((mo mappable-object))
  nil)

(defclass-f roche-lobe (mappable-object)
  ((x0 :accessor x0)))

(defclass-f primary-roche-lobe (roche-lobe)
  ((name :initform "Primary Roche lobe")
   (x0 :initform 0.0)))

(defclass-f secondary-roche-lobe (roche-lobe)
  ((name :initform "Secondary Roche lobe")
   (x0 :initform 1.0)))

(lazy-slot contour ((lb roche-lobe))
  (let ((cont (get-roche-lobe (qprop (source lb)) (x0 lb))))
    (setf (cdr (last cont)) (list (car cont)))
    cont))


(defmethod-f xy-to-v ((lb roche-lobe))
  (let ((qprop (qprop (source lb)))
        (aomega (aomega (source lb))))
    (lambda (xy)
      (let* ((xmc (/ qprop (1+ qprop)))
             (x (- (car xy) xmc))
             (y (cdr xy)))
        (cons (* -1 aomega y) (* aomega x))))))

(defclass-f accretion-disk (mappable-object)
  ((x0 :accessor x0)
   (ecc :accessor ecc
        :initform 0)
   (angle :accessor angle
          :initform 0)
   (qprop)
   (rd)))

(lazy-slot qprop ((d accretion-disk))
  (qprop (source d)))

(lazy-slot rd ((d accretion-disk))
  (/ 0.6 (1+ (qprop d))))

(lazy-slot contour ((d accretion-disk))
  (let* ((rd (rd d))
         (x0 (x0 d))
         (np 100)
         (ecc (ecc d))
         (ang (* pi -1 (1+ (/ (angle d) -180))))
         (p (* rd (- 1 (sqr ecc))))
         (cont (loop for i below np collect
                 (let* ((phi (+ (* 2 pi (/ i np))))
                        (r (/ p (1+ (* ecc (jscos (+ ang phi)))))))
                   (cons (+ x0 (* r (jscos phi)))
                         (* r (jssin phi)))))))
    `(,@cont ,(car cont))))

(defmethod-f get-v-path ((d accretion-disk) &key yofs)
  (let ((vmax (* 2 (max-v (params (source d))))))
    `(:|d| ,(format nil "~A M ~A,~A h ~A v ~A h ~A z"
               (get-path d :space :v :yofs yofs)
               (- vmax) (- vmax)
               (* 2 vmax)
               (* 2 vmax)
               (* -2 vmax)))))

(defmethod-f xy-to-v ((d accretion-disk))
  (let* ((src (source d))
         (qprop-src (qprop src))
         (qprop-dsk (qprop d))
         (aomega (aomega src))
         (x0 (x0 d))
         (rd (rd d))
         (ang (angle d))
         (ang (* -1 pi (1+ (/ ang -180))))
         (ecc (ecc d))
         (xmc (/ qprop-src (1+ qprop-src))))
    (lambda (xy)
      (let* ((x (- (car xy) x0))
             (y (cdr xy))
             (r (sqrt (+ (sqr x) (sqr y))))
             (phi (+ ang (jsatan2 y x)))
             (a (/ (* r (1+ (* ecc (jscos phi))))
                   (- 1 (sqr ecc))))
             (fpa (jsatan2 (* ecc (jssin phi)) (1+ (* ecc (jscos phi)))))
             (fa (- (+ phi (* 0.5 pi)) fpa ang)))
        (let ((vk (* aomega
                     (sqrt (* (/ (sqrt (* qprop-src qprop-dsk))
                                 (1+ qprop-src))
                              (- (/ 2 r) (/ 1 a)))))))
          (cons (* vk (jscos fa))
                (+ (* aomega x0) (* vk (jssin fa)))))))))

(defun get-fpa (phi e)
  (let ((phi (* phi 2 pi)))
    (acos (/ (1+ (* e (cos phi)))
             (sqrt (+ 1 (* e e) (* 2 e (cos phi))))))))


(defmethod-f inside ((d accretion-disk) x y) ;; Speedup the check a bit for circular disks
  (if (= (ecc d) 0)
      (<= (sqrt (+ (sqr (- x (x0 d))) (sqr y)))
          (rd d))
      (call-next-method)))

(defmethod-f setup-elements ((d accretion-disk))
  (labels ((update-all ()
             (slot-makunbound d 'contour)
             (slot-makunbound d 'bounds)
             (slot-makunbound d 'xy-cnv)
             (slot-makunbound d 'v-map)
             (map-intensity d)
             (ignore-errors (redraw (xy-plot d)))
             ; (when (selected d)
             ;   (multiple-value-bind (xmin xmax ymin ymax) (bounds d)
             ;     (rescale (parent (xy-plot d)) :xmin xmin :xmax xmax :ymin ymin :ymax ymax)))
             (ignore-errors (redraw (v-plot d)))))
    (list (create-element "table"
            :append-element
              (create-element "tr"
                :append-element (create-element "td" :|innerHTML| "Major semi-axis (Rsun):")
                :append-element
                  (create-element "td"
                    :append-element
                      (macrolet ((cf ()
                                   '(* 4.217
                                       (expt (* (sqr (period (source d)))
                                                (+ (primary-mass (source d))
                                                   (secondary-mass (source d))))
                                             (/ 1 3)))))
                        (render-widget (make-instance 'editable-field
                                         :value (format nil "~A" (* (rd d) (cf)))
                                         :ok (lambda (val)
                                               (let ((v (js-parse-float val)))
                                                 (when (and v (not (is-nan v)) (> v 0))
                                                   (setf (slot-value d 'rd) (/ v (cf)))
                                                   (update-all)
                                                   v))))))))
            :append-element
              (create-element "tr"
                :append-element (create-element "td" :|innerHTML| "Eccentricity:")
                :append-element
                  (create-element "td"
                    :append-element (render-widget (make-instance 'editable-field
                                                     :value (ecc d)
                                                     :ok (lambda (val)
                                                           (let ((v (js-parse-float val)))
                                                             (when (and v (not (is-nan v)) (>= v 0) (< v 1))
                                                               (setf (slot-value d 'ecc) v)
                                                               (update-all)
                                                               v)))))))
            :append-element
              (create-element "tr"
                :append-element (create-element "td" :|innerHTML| "Position angle (deg):")
                :append-element
                  (create-element "td"
                    :append-element (render-widget (make-instance 'editable-field
                                                     :value (angle d)
                                                     :ok (lambda (val)
                                                           (let ((v (js-parse-float val)))
                                                             (when (and v (not (is-nan v)))
                                                               (setf (slot-value d 'angle) v)
                                                               (update-all)
                                                               v)))))))))))

(defclass-f primary-disk (accretion-disk)
  ((x0 :initform 0)
   (name :initform "Primary accretion disk")))

(defclass-f secondary-disk (accretion-disk)
  ((x0 :initform 1)
   (name :initform "Secondary accretion disk")))

(lazy-slot qprop ((d secondary-disk))
  (/ 1 (qprop (source d))))

(defmethod-f aomega ((s real-profile-source))
  (* 212.9 (expt (/ (+ (secondary-mass s) (primary-mass s))
                    (period s))
                 (/ 1 3))))

(defmethod-f qprop ((s real-profile-source))
  (/ (secondary-mass s) (primary-mass s)))

(defclass-f picker-graph (saveable-graph)
   ((to-hide :initform nil
             :initarg :to-hide
             :accessor to-hide)
    (hidden-states :initform nil
                   :accessor hidden-states
                   :initarg :hidden-states)
    (update-fn :initform (constantly nil)
               :initarg :update-fn
               :accessor update-fn)
    (hls :accessor hls
         :initform nil)))

(defmethod-f save-dialog-enter ((p picker-graph))
  (let ((mol (remove-if #'null (mapcar (lambda (p) (ignore-errors (source p))) (plots p)))))
    (setf (slot-value p 'hls)
          (mapcar #'cons
                  mol
                  (mapcar #'highlited mol)))
    (map nil (lambda (m)
               (setf (slot-value m 'highlited) nil)
               (map nil #'redraw `(,(v-plot m) ,(xy-plot m))))
             mol))
  (setf (slot-value p 'hidden-states)
        (mapcar (lambda (obj)
                  (prog1
                    (cons obj (jscl::oget obj "style" "visibility"))
                    (setf (jscl::oget obj "style" "visibility") "hidden")))
                (to-hide p))))

(defmethod-f save-dialog-leave ((p picker-graph))
  (map nil (lambda (obj vis)
             (setf (jscl::oget obj "style" "visibility") vis))
           (mapcar #'car (hidden-states p))
           (mapcar #'cdr (hidden-states p)))
  (map nil (lambda (m h)
             (setf (slot-value m 'highlited) h)
             (map nil #'redraw `(,(v-plot m) ,(xy-plot m))))
           (mapcar #'car (hls p))
           (mapcar #'cdr (hls p))))

(defmethod-f redraw ((p picker-graph))
  (prog1
    (call-next-method)
    (funcall (update-fn p))))

(defmethod-f get-controls ((s real-profile-source) node &optional img matrix)
  (macrolet ((make-label (name add &rest code)
               (let ((app-block (gensym)))
                 `(let ((,app-block (create-element "div" :|style.display| "none"
                                                          :|style.marginLeft| "2em"
                                     :append-elements ,add)))
                    (with-self div
                      (create-element "div" :|style.width| "100%"
                        :append-element
                          (create-element "div"
                            :append-element
                              (create-element "label"
                                :append-element (create-element "span" :|style.marginRight| "1em"
                                                                       :|style.textDecorationStyle| "dashed"
                                                                       :|style.textDecorationLine| "underline"
                                                                       :|style.textDecorationThicknes| "1.75pt"
                                                                       :|style.color| "blue"
                                                                       :|innerHTML| ,name)
                                :append-element
                                  (with-self box
                                    (create-element "input" :|type| "checkbox"
                                      :|onclick| (lambda (ev)
                                                   (let ((checked (jscl::oget box "checked")))
                                                     (setf (jscl::oget ,app-block "style" "display") (if checked "block" "none"))
                                                     ,@code)
                                                   t)))))
                        :append-elements (if ,add (list ,app-block)))))))
             (with-xy (ev &rest code)
               (let ((xmin (gensym))
                     (xmax (gensym))
                     (ymin (gensym))
                     (ymax (gensym))
                     (wi (gensym))
                     (he (gensym)))
                 `(with-slots ((,xmin xmin) (,xmax xmax) (,ymin ymin) (,ymax ymax)) picker-grf
                    (oget-bind (x y) ,ev ("layerX" "layerY")
                      (oget-bind (,wi ,he) grf ("clientWidth" "clientHeight")
                        (destructuring-bind (x y) (mapcar (lambda (x w mi ma)
                                                            (+ mi (* (- ma mi) (/ x w))))
                                                          (list x (- ,he y))
                                                          (list ,wi ,he)
                                                          (list ,xmin ,ymin)
                                                          (list ,xmax ,ymax))
                          (when (and (> x ,xmin)
                                     (< x ,xmax)
                                     (> y ,ymin)
                                     (< y ,ymax))
                            ,@code))))))))
    (let* ((picker-hint (create-element "div" :|style.margin| "0.5em"
                                              :|style.position| "absolute"
                                              :|style.right| 0
                                              :|style.top| 0
                                              :|style.fontSize| "0.75em"))
           (vmax (max-v (params s)))
           (cf (/ 100 (* 2 vmax))) ;; 100 is a 100%
           (igrf-on nil)
           (cur-mo nil)
           (mapping-mo nil)
           (last-bnd nil)
           (igrf (create-element "div" :|style.position| "absolute"
                                       :|style.width| "20px"
                                       :|style.height| "20px"
                                       :|style.transform| "translate(-10px,-10px)"
                                       :|style.visibility| "hidden"
                                       :|style.z-index| 5
                                       ; :|style.filter| "drop-shadow(-0.5px -0.5px 1px white) drop-shadow(0.5px 0.5px 1px white) drop-shadow(-0.5px 0.5px 1px white) drop-shadow(0.5px -0.5px 1px white);"
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
           (clear-btn (create-element "button" :|innerHTML| "clear"
                                               :|style.position| "absolute"
                                               :|style.top| "0.25em"
                                               :|style.right| "0.5em"
                                               :|style.zIndex| 200
                         :|onclick| (lambda (ev)
                                      (let* ((xy-cnv (xy-cnv mapping-mo))
                                             (xy-ctx ((jscl::oget xy-cnv "getContext") "2d"))
                                             (vmres (v-map-res mapping-mo))
                                             (xy-idat ((jscl::oget xy-ctx "getImageData") 0 0 vmres vmres))
                                             (xy-dat (jscl::oget xy-idat "data"))
                                             (resolution (resolution (params s)))
                                             (v-cnv (v-cnv mapping-mo))
                                             (v-ctx ((jscl::oget v-cnv "getContext") "2d"))
                                             (v-idat ((jscl::oget v-ctx "getImageData") 0 0 resolution resolution))
                                             (v-dat (jscl::oget v-idat "data")))
                                        (loop for i below (* 4 (sqr vmres)) do (setf (aref xy-dat i) 0))
                                        (loop for i below (* 4 (sqr resolution)) do (setf (aref v-dat i) 0))
                                        ((jscl::oget v-ctx "putImageData") v-idat 0 0)
                                        ((jscl::oget xy-ctx "putImageData") xy-idat 0 0))
                                      (redraw (xy-plot mapping-mo))
                                      ((jscl::oget ev "stopImmediatePropagation")))))
           (mappable-classes '(primary-roche-lobe
                               secondary-roche-lobe
                               primary-disk
                               secondary-disk))
           (mappable-objects (mapcar (lambda (cls z) (make-instance cls :source s :matrix matrix :zindex z))
                                     mappable-classes
                                     (loop for i below (length mappable-classes) collect (+ 5 (* i 5)))))
           (picker-grf (make-instance 'picker-graph :xmin -1 :xmax 2 :ymin -1 :ymax 1 :preserve-aspect-ratio t
                                                    :to-hide (list clear-btn picker-hint)))
           (picker-div (create-element "div" :|style.textAlign| "center"
                                             :|style.display| "none"
                                             :|style.width| "100%"
                                             :|style.marginTop| "2em"
                         :append-element
                           (create-element "span" :|style.width| "80%"
                                                  :|style.display| "inline-block"
                             :append-element (render-widget picker-grf)))))
      (ensure-element (if (slot-boundp img 'graph) (graph img))
        (append-element igrf (graph img)))
      (labels ((set-picker-decoration ()
                 (let* ((grf (graph picker-grf)))
                   (append-element picker-hint grf)
                   (when mapping-mo
                     (append-element clear-btn grf))
                   (setf (jscl::oget grf "onclick")
                         (lambda (ev)
                           (when (not (in-save-dialog picker-grf))
                             (if mapping-mo
                                 (with-xy ev
                                   (when (not (inside mapping-mo x y))
                                     (destructuring-bind (xmin xmax ymin ymax) last-bnd
                                       (rescale picker-grf :xmin xmin :xmax xmax :ymin ymin :ymax ymax))
                                     (remove-element (v-cnv mapping-mo))
                                     (setf (slot-value mapping-mo 'selected) nil)
                                     (setf mapping-mo nil)
                                     (set-picker-decoration)))
                                 (when cur-mo
                                   (setf mapping-mo cur-mo)
                                   (setf (slot-value mapping-mo 'selected) t)
                                   (append-element (v-cnv mapping-mo) (graph img))
                                   (setf (jscl::oget picker-hint "innerHTML") (format nil "~A (click outside to unzoom)" (name mapping-mo)))
                                   (setf last-bnd (loop for i in `(,#'xmin ,#'xmax ,#'ymin ,#'ymax) collect (funcall i picker-grf)))
                                   (destructuring-bind (xmin xmax ymin ymax) (bounds mapping-mo)
                                     (let ((dw (* 0.1 (- xmax xmin)))
                                           (dh (* 0.1 (- ymax ymin))))
                                       (rescale picker-grf :xmin (- xmin dw) :xmax (+ xmax dw) :ymin (- ymin dh) :ymax (+ ymax dh))
                                       (set-picker-decoration))))))))
                   (setf (jscl::oget grf "onmousemove")
                         (lambda (ev)
                           (when (and (not (in-save-dialog picker-grf))
                                      (equal "http://www.w3.org/2000/svg" (jscl::oget ev "target" "namespaceURI")))
                             (with-xy ev
                               (setf cur-mo (loop for mo in (reverse mappable-objects)
                                              when (and (active mo) (inside mo x y)) return mo))
                               (loop for mo in mappable-objects do
                                 (when (not (equal (equal mo cur-mo) (highlited mo)))
                                   (setf (slot-value mo 'highlited) (equal mo cur-mo))
                                   (mapcar #'redraw `(,(xy-plot mo) ,(v-plot mo)))
                                   (when (not mapping-mo)
                                     (if (highlited mo)
                                         (append-element (v-cnv mo) (graph img))
                                         (remove-element (v-cnv mo))))))
                               (setf igrf-on nil)
                               (when cur-mo
                                 (when (not mapping-mo)
                                   (setf (jscl::oget picker-hint "innerHTML") (format nil "~A (click for zoom/map mode)" (name cur-mo))))
                                 (let ((v (funcall (xy-to-v cur-mo) (cons x y))))
                                   (when v
                                     (setf (jscl::oget igrf "style" "left") (format nil "~A%" (* cf (+ (car v) vmax))))
                                     (setf (jscl::oget igrf "style" "top") (format nil "~A%" (* cf (- vmax (cdr v)))))
                                     (setf igrf-on t))))
                               (setf (jscl::oget igrf "style" "visibility") (if igrf-on "visible" "hidden"))
                               (if (and (not igrf-on) (not mapping-mo))
                                   (setf (jscl::oget picker-hint "innerHTML") "")))))))))
        (funcall (setf (slot-value picker-grf 'update-fn) #'set-picker-decoration)))
      `(,picker-div
        ,(create-element "div" :|style.display| "inline-block"
           :append-element (make-label "Show picker:" nil
                            (setf (jscl::oget picker-div "style" "display") (if checked "inline-block" "none")))
           :append-elements (mapcar
                              (lambda (mo)
                                (make-label (format nil "Show ~A" (name mo)) (setup-elements mo)
                                  (setf (slot-value mo 'active) checked)
                                  (if checked
                                      (map nil #'add-plot `(,picker-grf ,img) `(,(xy-plot mo) ,(v-plot mo)))
                                      (map nil #'remove-plot `(,(xy-plot mo) ,(v-plot mo))))))
                              mappable-objects))))))
