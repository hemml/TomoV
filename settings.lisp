(in-package :tomo)

(defclass-f watcher ()
  ((params :initarg :params
           :accessor params)))

(defclass-f parameters (modal-dialog-window)
  ((fwhm :initform 100.0
         :accessor fwhm)
   (max-v :initform 1500.0
          :accessor max-v)
   (resolution :initform 50
               :accessor resolution)
   (watchers :initform nil
             :accessor watchers)))

(defmethod-f update-params ((w watcher) (p parameters)))
  ; (error "You have to override update-params nmethod!"))

(defmethod-f add-watcher ((p parameters) (w watcher))
  (let ((wl (watchers p)))
    (pushnew w wl)
    (setf (slot-value p 'watchers) wl)))

(defmethod-f update-all-watchers ((p parameters))
  (loop for w in (watchers p) do (update-params w p)))

(defclass-f psf-graph (graph watcher)
  ())

(defmethod-f initialize-instance :after ((obj watcher) &rest args)
  (add-watcher (params obj) obj))

(defmethod-f update-params ((g psf-graph) (p parameters))
  (remove-all-plots g)
  (add-plot g (make-instance 'func-plot
                :func (let ((sigma (/ (fwhm p) (sqrt (* 8.0 (jsln 2.0))))))
                        (lambda (x)
                          (exp (/ (* -0.5 x x) (* sigma sigma)))))))
  (rescale g :xmin (- (* 2 (fwhm p))) :xmax (* 2 (fwhm p))))

(defmethod-f render-widget :after ((p parameters))
  (append-element
    (labels ((mkfld (name units def fld)
                (create-element "div" :|style.width| "100%"
                                      :|style.display| "inline-block"
                                      :|style.marginTop| "1em"
                  :append-element (create-element "span" :|style.marginRight| "0.5em"
                                     :append-element name)
                  :append-element
                    (create-element "span"
                            :append-element
                              (render-widget (make-instance 'editable-field
                                               :input-size 5
                                               :value def
                                                 :ok (lambda (v)
                                                       (let ((v1 (js-parse-float v)))
                                                         (if (> v1 0)
                                                             (progn
                                                               (setf (slot-value p fld) v1)
                                                               (update-all-watchers p)
                                                               v1)))))))
                  :append-element (create-element "span" :|style.marginLeft| "1em"
                                    :append-element units))))
      (create-element "div" :|style.float| "left"
                            :|style.width| "16em"
                            :|style.textAlign| "center"
                            :|style.border| "1px solid black"
                            :|style.paddingBottom| "1em"
                            :|style.borderRadius| "0.25em"
                            :|style.background| "white"
        :append-element (create-element "div" :|style.width| "100%"
                                              :|style.marginBottom| "1em"
                                              :|style.textAlign| "center"
                                              :|style.background| "rgb(255,184,132)"
                                              :|style.borderBottom| "1px solid black"
                                              :|style.paddingTop| "0.2em"
                                              :|style.paddingBottom| "0.2em"
                          :append-elements "Settings")

        :append-element
          (create-element "div" :|style.width| "80%"
                                :|style.aspectRatio| 1
                                :|style.display| "inline-block"
            :append-element
              (let ((psf (make-instance 'psf-graph :params p :xmin (* -2 (fwhm p)) :xmax (* 2 (fwhm p)) :ymin 0 :ymax 1 :scales '(:bottom) :xcaption "V (km/s)")))
                (update-params psf p)
                (render-widget psf)))
        :append-element (mkfld "FWHM:" "km/s" (fwhm p) 'fwhm)
        :append-element (mkfld "Max V:" "km/s" (max-v p) 'max-v)
        :append-element (mkfld "Resolution:" "pixels" (resolution p) 'resolution)
        :append-element
          (create-element "button" :|style.width| "8em"
                                   :|style.marginTop| "1em"
                                   :|style.display| "inline-block"
                                   :|innerHTML| "close"
            :|onclick| (lambda (ev) (close p)))))
    (root p)))
