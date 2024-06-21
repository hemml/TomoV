(in-package :tomo)

(defclass-f saveable-graph (graph)
  ((name :initform nil
         :initarg :name
         :accessor name)
   (in-save-dialog :initform nil
                   :accessor in-save-dialog)))

(defclass-f save-image-dialog (modal-dialog-window)
  ((element :accessor element
            :initarg :element)))

(defmethod-f save-dialog-enter ((g saveable-graph)))
(defmethod-f save-dialog-leave ((g saveable-graph)))

(defmethod-f save-dialog-enter :after ((g saveable-graph))
  (setf (slot-value g 'in-save-dialog) t))

(defmethod-f save-dialog-leave :after ((g saveable-graph))
  (setf (slot-value g 'in-save-dialog) nil))


(defmethod-f render-widget :after ((sd save-image-dialog))
  (let* ((elsd (element sd))
         (element (root elsd)))
    (save-dialog-enter elsd)
    (oget-bind (gw gh) element ("clientWidth" "clientHeight")
      (let* ((graph (graph elsd))
             (parent (parent-element element))
             (prev (jscl::oget element "nextSibling"))
             (scal (* 0.6 (page-width)))
             (v0 (/ gw scal))
             (resol 1000)
             (root-svg (make-svg :|viewBox| (format nil "0 0 ~A ~A" (* scal v0) (* v0 scal (/ gh gw)))
                                 :|style.width| "100%"
                        `(:|foreignObject| :|style.width| "calc(100% - 1px)"
                                           :|style.height| "calc(100% - 1px)"
                            ,(create-element "div" :|style.width| "calc(100% - 1px)"
                                                   :|style.height| "calc(100% - 1px)"
                                                   :|style.position| "relative"
                                                   :|style.top| "1px"
                                                   :xmlns "http://www.w3.org/1999/xhtml"
                               :append-element element)))))
        (labels ((get-mime (ext)
                   (cond ((equal "png" ext) "image/png")
                         ((equal "svg" ext) "image/svg+xml;charset=utf-8")
                         ((or (equal "jpg" ext)
                              (equal "jpeg" ext))
                          "image/jpeg"))))
          (destructuring-bind (full-name mime-type) (let* ((nam (if (and (slot-boundp elsd 'name) (name elsd)) (name elsd) "image"))
                                                           (pos (position #\. nam :from-end t))
                                                           (ext (if pos (subseq nam (1+ pos)) "png")))
                                                      (list (format nil "~A.~A" (if pos (subseq nam 0 pos) nam) ext)
                                                            (get-mime (string-downcase ext))))
            (labels ((get-svg ()
                       (format nil "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~A\" height=\"~A\" viewBox=\"0 0 ~A ~A\" style=\"width: 100%; height: 100%; background: white;\">~A</svg>"
                                     (jscl::oget root-svg "viewBox" "baseVal" "width") (jscl::oget root-svg "viewBox" "baseVal" "height")
                                     (jscl::oget root-svg "viewBox" "baseVal" "width") (jscl::oget root-svg "viewBox" "baseVal" "height")
                                     (jscl::oget root-svg "innerHTML")))
                     (get-svg-url ()
                       (let* ((svg (get-svg))
                              (blob (jscl::make-new (winref "Blob")
                                      (jscl::make-new (winref "Array")
                                        (jscl::lisp-to-js svg))
                                      (make-js-object :|type| "image/svg+xml;charset=utf-8"))))
                         ((jscl::oget (jscl::lisp-to-js (jscl::%js-vref "URL")) "createObjectURL") blob)))
                     (save (url)
                       (let ((a (create-element "a" :href url :download full-name)))
                         (append-element a)
                         ((jscl::oget a "click"))
                         (remove-element a)))
                     (make-edit-fld (name &key slot
                                               (init (slot-value elsd slot))
                                               (convert #'identity)
                                               (err-check (constantly nil))
                                               (set-val (lambda (v) (when slot (setf (slot-value elsd slot) v))))
                                               (after (lambda (v))))
                       (let ((err (create-element "span" :|style.color| "red"
                                                         :|style.marginLeft| "2em"
                                                         :|style.whiteSpace| "nowrap")))
                         (labels ((mkerr (txt)
                                    (setf (jscl::oget err "innerHTML")
                                          (if txt
                                              (format nil "ERROR: ~A" txt)
                                              ""))
                                    nil))
                           (create-element "tr"
                             :append-element
                               (create-element "td" :align "right"
                                                    :|style.paddingRight| "1em"
                                                    :|style.whiteSpace| "nowrap"
                                                    :|innerHTML| name)
                             :append-element
                               (create-element "td" :align "center"
                                                    :|style.paddingRight| "1em"
                                 :append-element
                                   (render-widget
                                     (make-instance 'editable-field :value init
                                       :cancel (lambda (val) (mkerr nil))
                                       :ok (lambda (val)
                                             (let* ((v (funcall convert val))
                                                    (errstr (funcall err-check v)))
                                               (mkerr errstr)
                                               (when (not errstr)
                                                 (funcall set-val v)
                                                 (funcall after v)
                                                 val))))))
                             :append-element
                               (create-element "td" :align "left"
                                                    :|style.paddingLeft| "2em"
                                 :append-element err))))))
              (macrolet ((with-image-url (url &rest code)
                           (let ((img (gensym))
                                 (cnv (gensym))
                                 (ctx (gensym)))
                             `(with-self ,img
                                (create-element "img" :|style.width| "100%"
                                                      :|style.height| "100%"
                                                      :src (format nil "data:image/svg+xml;charset=utf-8;base64,~A" (funcall (winref "btoa") (jscl::lisp-to-js (get-svg))))
                                  :|onload| (lambda (,(gensym))
                                              (oget-bind (w h) root-svg ("clientWidth" "clientHeight")
                                                (let* ((,cnv (create-element "canvas" :|width| resol
                                                                                      :|height| (floor (* resol (/ h w)))))
                                                       (,ctx ((jscl::oget ,cnv "getContext") "2d")))
                                                  ((jscl::oget ,ctx "drawImage") ,img 0 0 resol (floor (* resol (/ h w))))
                                                  (let ((,url ((jscl::oget ,cnv "toDataURL") mime-type)))
                                                    ,@code)))))))))
                (append-element
                  (create-element "div" :|style.background| "white"
                                        :|style.border| "1px solid black"
                                        :|style.borderRadius| "0.5em"
                                        :|style.padding| "1em"
                                        :|style.display| "flex"
                                        :|style.flexFlow| "row"
                    :append-element
                      (with-self div
                        (ensure-element div
                          (setf (jscl::oget div "style" "width") "100%"))
                        (create-element "div" :|style.width| "99%"
                                              :|style.height| "fit-content"
                                              :|style.border| "1px dashed black"
                          :append-element root-svg))
                    :append-element
                      (create-element "div" :|style.padding| "3em"
                        :append-element
                          (create-element "table" :|style.fontSize| "1.5em"
                            :append-element (make-edit-fld "Pixel scale:"
                                              :init 1
                                              :convert #'js-parse-float
                                              :err-check (lambda (v)
                                                           (when (not (and v (not (is-nan v)) (> v 0)))
                                                             "it must be a number >0"))
                                              :set-val (lambda (v)
                                                         (labels ((upd ()
                                                                    (oget-bind (gw gh) element ("clientWidth" "clientHeight")
                                                                      (setf (jscl::oget root-svg "viewBox" "baseVal" "width")
                                                                            (* v0 (/ scal v)))
                                                                      (setf (jscl::oget root-svg "viewBox" "baseVal" "height")
                                                                            (* v0 (/ scal v) (/ gh gw))))))
                                                           (upd)
                                                           ((jscl::oget (jscl::%js-vref "self") "requestAnimationFrame")
                                                            (lambda (ev)
                                                              (execute-after 0.01 #'upd))))))

                            :append-element
                              (create-element "tr"
                                :append-element
                                  (create-element "td" :align "right"
                                                       :|style.paddingRight| "1em"
                                                       :|innerHTML| "Final resolution:")
                                :append-element
                                  (let ((x-res (make-instance 'editable-field :value resol))
                                        (y-res (make-instance 'editable-field :value (floor (* resol (/ gh gw))))))
                                    (setf (slot-value x-res 'ok)
                                          (lambda (val)
                                            (let ((v (js-parse-float val)))
                                              (when (and v (not (is-nan v)) (> v 0))
                                                (setf (slot-value y-res 'value) (floor (* (setf resol v) (/ gh gw))))
                                                (redraw y-res)
                                                v))))
                                    (setf (slot-value y-res 'ok)
                                          (lambda (val)
                                            (let ((v (js-parse-float val)))
                                              (when (and v (not (is-nan v)) (> v 0))
                                                (setf (slot-value x-res 'value)
                                                      (setf resol (floor (* v (/ gw gh)))))
                                                (redraw x-res)
                                                v))))
                                    (create-element "td" :align "center"
                                                         :|style.paddingRight| "1em"
                                      :append-element
                                        (render-widget x-res)
                                      :append-element (create-element "span" :|innerHTML| "x"
                                                                             :|style.marginLeft| "1em"
                                                                             :|style.marginRight| "1em")
                                      :append-element (render-widget y-res)))

                                  :append-element (create-element "td"))
                            :append-element
                              (labels ((get-nam-typ (val)
                                         (let* ((dot-pos (position #\. val :from-end t))
                                                (name (if dot-pos (subseq val 0 dot-pos)))
                                                (typ (if dot-pos (get-mime (subseq val (1+ dot-pos))))))
                                           (values name typ))))
                                (make-edit-fld "Image name:"
                                               :init full-name
                                               :err-check (lambda (v)
                                                            (multiple-value-bind (name typ) (get-nam-typ v)
                                                              (cond ((not name) "specify a name with extension")
                                                                    ((not typ) "the extension must be .png, .jpg/.jpeg or .svg"))))
                                               :set-val (lambda (v)
                                                          (multiple-value-bind (name typ) (get-nam-typ v)
                                                            (setf mime-type typ)
                                                            (setf full-name (setf (slot-value elsd 'name) v))))))
                            :append-elements
                              (mapcan (lambda (slot name)
                                        (if (or (not (fboundp slot))
                                                (let ((v (ignore-errors (funcall slot elsd))))
                                                  (or (not v)
                                                      (equal (slot-value elsd slot) v))))
                                            (list (make-edit-fld name :slot slot :convert #'js-parse-float
                                                    :err-check (lambda (v) (if (not (and v (not (is-nan v))))
                                                                               "please specify a number"))
                                                    :after (lambda (v) (setf element (redraw elsd)))))))
                                      '(xmin xmax ymin ymax)
                                      '("X<sub>min</sub>"
                                        "X<sub>max</sub>"
                                        "Y<sub>min</sub>"
                                        "Y<sub>max</sub>"))
                            :append-elements
                              (mapcan (lambda (slot name)
                                        (if (or (not (fboundp slot))
                                                (let ((v (ignore-errors (funcall slot elsd))))
                                                  (or (not v)
                                                      (equal (slot-value elsd slot) v))))
                                            (list (make-edit-fld name :slot slot :convert #'js-parse-float
                                                    :err-check (lambda (v) (if (not (and v
                                                                                         (not (is-nan v))
                                                                                         (>= v 0)
                                                                                         (= (floor v) v)))
                                                                               "please specify an integer > 0"))
                                                    :after (lambda (v) (setf element (redraw elsd)))))))
                                      '(xticks yticks)
                                      '("X<sub>ticks</sub>"
                                        "Y<sub>ticks</sub>"))
                            :append-elements
                              (mapcan (lambda (slot name)
                                        (if (or (not (fboundp slot))
                                                (let ((v (ignore-errors (funcall slot elsd))))
                                                  (or (not v)
                                                      (equal (slot-value elsd slot) v))))
                                            (list (make-edit-fld name :slot slot :convert #'js-parse-float
                                                    :err-check (lambda (v) (if (not (and v
                                                                                         (not (is-nan v))
                                                                                         (>= v 0)))
                                                                               "please specify number > 0"))
                                                    :after (lambda (v) (setf element (redraw elsd)))))))
                                      '(xdelta ydelta)
                                      '("X<sub>delta</sub>"
                                        "Y<sub>delta</sub>"))
                            :append-element
                              (create-element "tr"
                                :append-element
                                  (create-element "td" :colspan 2
                                    :append-element
                                      (create-element "button" :|innerHTML| "save image"
                                                               :|style.margin| "1em"
                                                               :|style.width| "100%"
                                        :|onclick| (lambda (ev)
                                                      (if (search "image/svg" mime-type)
                                                          (save (get-svg-url))
                                                          (with-image-url url
                                                            (save url)))))))))
                    :append-element
                      (create-element "button" :|innerHTML| "close"
                                               :|style.position| "absolute"
                                               :|style.top| "0.5em"
                                               :|style.right| "0.5em"
                        :|onclick| (lambda (ev)
                                     ((jscl::oget parent "insertBefore") (root elsd) prev)
                                     (save-dialog-leave elsd)
                                     (close sd))))
                  (root sd))))))))))

(defmethod-f add-save-element ((g saveable-graph))
  (append-element
    (with-self label
      (add-event-listener "mouseenter"
        (lambda (ev)
          (when (not (in-save-dialog g))
            (setf (jscl::oget label "style" "display") "inline-block")))
        :element (root g))
      (add-event-listener "mouseleave"
        (lambda (ev)
          (setf (jscl::oget label "style" "display") "none"))
        :element (root g))
      (create-element "a" :|href| "#"
                          :|innerHTML| "save image"
                          :|style.position| "absolute"
                          :|style.right| 0
                          :|style.bottom| 0
                          :|style.zIndex| "1000"
                          :|style.display| "none"
        :|onclick| (lambda (ev)
                     (setf (jscl::oget label "style" "display") "none")
                     (append-element
                       (render-widget (make-instance 'save-image-dialog :element g))))))
    (graph g)))

(defmethod-f render-widget :after ((g saveable-graph))
  (add-save-element g))
