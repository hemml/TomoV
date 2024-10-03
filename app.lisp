(in-package :tomo)

;(setf omg::*ssl-key* #P"privkey1.pem")
;(setf omg::*ssl-cert* #P"fullchain1.pem")

(defparameter-f *app* nil)

(defparameter-f *registry* nil)

(defclass-f app (omg-widget)
  ((lst :initform (make-instance 'obj-list)
        :accessor lst)
   (main-root :accessor main-root
              :initform
                (create-element "div" :|style.padding| "0.5em"))))

(defclass-f obj-list (omg-widget)
  ((items :initform nil
          :accessor items)))

(defclass-f obj-item (omg-widget)
  ((name :accessor name
         :initarg :name)))

(defclass-f choice-dialog (modal-dialog-window)
  ((main-root :accessor main-root
              :initarg :main-root)
   (variants :accessor variants
             :initarg :variants)))

(defmethod-f render-widget ((o obj-item))
  (setf (slot-value o 'root)
        (create-element "div" :|innerHTML| (name o)
                              :|style.paddingLeft| "1em"
                              :|style.paddingRight| "1em"
                              :|style.paddingTop| "0.25em"
                              :|style.paddingBottom| "0.25em"
                              ; :|style.marginBottom| "0.5em"
                              ; :|style.marginTop| "0.5em"
          :add-style ":hover {cursor: pointer;background: #f0f0ff}"
          :|onclick| (lambda (ev)
                       (indexed-db-get (buf ("Tomo" "sources" (name o)) t)
                         (append-element
                           (render-widget (make-instance 'load-store-progress
                                            :buffer buf :label "Loading data:" :direction :in
                                            :final-cb (lambda (s n)
                                                        (source-loaded s)
                                                        (append-element (render-widget s) (main-root *app*)))))))))))

(defmethod-f render-widget ((l obj-list))
  (setf (slot-value l 'root)
        (create-element "div" :|style.width| "20em"
          :append-elements (mapcar #'render-widget (items l)))))

(defmethod-f update ((l obj-list) &key redraw)
  (indexed-db-get-all-keys (val ("Tomo" "sources"))
    (setf (slot-value l 'items)
          (mapcar
            (lambda (k)
              (make-instance 'obj-item :name k))
            (sort (map 'list #'identity val) #'string<)))
    (if redraw (redraw l))))

(defmethod-f initialize-instance :after ((l obj-list))
  (update l :redraw t))

(defmethod-f render-widget :after ((d choice-dialog))
  (append-element
    (create-element "div" :|style.textAlign| "center"
                          :|style.border| "1px solid black"
                          :|style.paddingBottom| "1em"
                          :|style.borderRadius| "0.25em"
                          :|style.width| "25em"
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
                          (create-element "span" :|innerHTML|"Select a data source:"
                                                 :|style.width| "100%"
                                                 :|style.display| "table-cell")
                        :append-element
                          (create-element "button" :|style.display| "table-cell"
                                                   :|style.marginRight| "0.25em"
                                                   :|innerHTML| "close"
                            :|onclick| (lambda (ev) (close d))))
      :append-elements
        (mapcar
          (lambda (v)
            (create-element "div" :|style.margin| "1em"
              :append-element (create-element "button" :|innerHTML| (car v)
                                                       :|style.width| "80%"
                                                       :|style.marginTop| "1em"
                                :|onclick| (let ((cls (cdr v)))
                                             (lambda (ev)
                                               (append-element (render-widget (make-instance cls))
                                                               (main-root d))
                                               (close d))))))
          (variants d)))
    (root d)))

(defclass-f save-as-dialog (modal-dialog-window)
  ((obj :accessor obj
        :initarg :obj)))

(defmethod-f render-widget :after ((w save-as-dialog))
  (let ((err (create-element "div" :|style.color| "red"
                                   :|style.textAlign| "center"
                                   :|style.marginTop| "0.5em"))
        (inp (create-element "input" :|style.width| "20em"
                                     :|style.textAlign| "center"
                                     :value (name (obj w)))))
    (append-element
      (create-element "div" :|style.border| "0.1em solid black"
                            :|style.border-radius| "0.5em"
                            :|style.padding| "0.5em"
                            :|style.background| "#fffff0"
        :append-element
          (create-element "span" :|style.paddingRight| "1em"
                                 :|style.textAlign| "center"
                                 :|innerHTML| "Save as:")
        :append-element inp
        :append-element err
        :append-element
          (create-element "div" :|style.textAlign| "center"
                                :|style.marginTop| "0.5em"
            :append-element (create-element "button" :|innerHTML| "save"
                              :|onclick| (lambda (ev)
                                           (indexed-db-get-all-keys (val ("Tomo" "sources"))
                                             (let ((name (jscl::oget inp "value")))
                                               (if (position name val :test #'equal)
                                                   (setf (jscl::oget err "innerHTML") "Image already exists!")
                                                   (let ((obj (obj w)))
                                                     (setf (slot-value obj 'name) name)
                                                     (close w)
                                                     (append-element
                                                       (render-widget (make-instance 'load-store-progress
                                                                        :obj obj :label "Saving data:" :direction :out
                                                                        :final-cb (lambda (buf siz)
                                                                                    (indexed-db-add "Tomo" "sources" name buf
                                                                                      :when-ok (lambda ()
                                                                                                 (update (lst *app*) :redraw t))
                                                                                      :raw t)))))))))))

            :append-element (create-element "button" :|innerHTML| "cancel"
                                                     :|style.marginLeft| "1em"
                              :|onclick| (lambda (ev)
                                           (close w)))))
      (root w))))

(defmethod-f render-widget ((a app))
  (setf (slot-value a 'root)
        (create-element "div" :|style.width| "100%"
                              :|style.position| "absolute"
                              :|style.left| 0
                              :|style.top| 0
                              :|style.height| "100%"
                              :|style.display| "flex"
                              :|style.flexFlow| "column"
                              :|style.font| "1.5em monospace"
          :append-element
            (create-element "div" :|style.width| "auto"
                                  :|style.display| "flex"
                                  :|style.flexFlow| "row"
                                  :|style.flex| "0 1 auto"
                                  :|style.height| "100%"
                                  :|style.left| 0
              :append-element
                (create-element "div" :|style.borderRight| "1px solid black"
                                      :|style.flex| "0 1 auto"
                                      :|style.width| "12em"
                  :append-element
                    (create-element "div" :|style.width| "100%"
                                          :|style.display| "flex"
                                          :|style.flexFlow| "row"
                                          :|style.height| "2.5em"
                                          :|style.borderBottom| "1px solid black"
                      :append-element
                        (create-element "button" :|innerHTML| "new image"
                                                 :|style.marginLeft| "1em"
                                                 :|style.marginRight| "1em"
                                                 :|style.marginTop| "0.5em"
                                                 :|style.marginBottom| "0.5em"
                                                 :|style.flex| "1 0 auto"
                          :|onclick| (lambda (ev)
                                       (append-element
                                         (render-widget
                                           (make-instance 'choice-dialog :main-root (main-root a)
                                                                         :variants '(("Sample data (letters)" . sample-profile-source)
                                                                                     ("Real data from files" . real-profile-source))))))))
                  :append-element
                    (create-element "div" :|style.width| "100%"
                                          :|style.height| "calc(100% - 2.5em)"
                                          :|style.overflowX| "scroll"
                                          :|style.overflowY| "scroll"
                                          :|style.flex| "0 1 auto"
                      :append-element (render-widget (lst a))
                      :append-element
                        (let ((files nil))
                          (with-self d
                            (create-element "div" :|innerHTML| "drop .tmv file here"
                                                  :|style.marginTop| "1em"
                                                  :|style.marginLeft| "1em"
                                                  :|style.marginRight| "1em"
                                                  :|style.textAlign| "center"
                                                  :|style.border| "2px dashed gray"
                                                  :|style.borderRadius| "0.25em"
                                                  :|style.padding| "1em 0 1em"
                                                  :|style.color| "gray"
                                                  :|style.background| "#fafafa"
                                                  :|style.fontWeight| "lighter"
                              :|ondragenter| (lambda (ev)
                                               (setf (jscl::oget d "style" "background") "#ffffa0")
                                               ; (jslog "F1" (jscl::oget ev "dataTransfer" "items"))
                                               ; (setf files (jscl::oget ev "dataTransfer" "items"))
                                               ; (jslog "FF" (jscl::oget files "length"))
                                               ((jscl::oget ev "preventDefault"))
                                               nil)
                              :|ondragleave| (lambda (ev)
                                               (setf (jscl::oget d "style" "background") "#fafafa")
                                               ((jscl::oget ev "preventDefault"))
                                               nil)
                              :|ondragover| (lambda (ev)
                                              ((jscl::oget ev "preventDefault"))
                                              nil)
                              :|ondrop| (lambda (ev)
                                          ((jscl::oget ev "preventDefault"))
                                          (loop for f across (jscl::oget ev "dataTransfer" "items") do
                                              ((jscl::oget ((jscl::oget ((jscl::oget f "getAsFile")) "arrayBuffer")) "then")
                                               (lambda (buf)
                                                 (append-element
                                                   (render-widget (make-instance 'load-store-progress
                                                                    :buffer buf :label "Loading data:" :direction :in
                                                                    :final-cb (lambda (src n)
                                                                                (append-element
                                                                                  (render-widget (make-instance 'save-as-dialog :obj src))))))))))
                                          (setf (jscl::oget d "style" "background") "#fafafa")
                                          (setf files nil)
                                          nil))))))

              :append-element
                (create-element "div"
                  :append-element (main-root a)
                  :append-element
                    (create-element "div" :|style.marginTop| "0.5em"
                                          :|style.marginLeft| "0.5em"
                                          :|style.fontSize| "0.75em"
                      :append-element
                        (create-element "div" :|innerHTML|
                          "In memory of <a href='https://ui.adsabs.harvard.edu/search/q=author%3A%22Tomov%2C%20N.%20A.%22&sort=date%20desc%2C%20bibcode%20desc&p_=0' target=_blank>Nikolay Tomov</a>, an astronomer and our friend.")
                      :append-element
                        (create-element "div" :|innerHTML|
                          "Sources are available at <a href='https://github.com/hemml/TomoV' target=_blank>https://github.com/hemml/TomoV</a> under a MIT License. A paper is in preparation.")
                      :append-element
                        (create-element "div" :|innerHTML|
                          "Supported by joint Russian-Bulgarian grant RFBR 20-52-18015 / KP-06-Russia/2-2020 (Bulgarian National Science Fund).")))))))

(defparameter-f *loading-banner* nil)

(defun-f show-loading ()
  (setf *loading-banner* ((jscl::oget (jscl::%js-vref "document") "createElement") "span"))
  (setf (jscl::oget *loading-banner* "innerHTML") "...")
  ((jscl::oget (jscl::%js-vref "document") "body" "appendChild") *loading-banner*)
  nil)

(defun-f hide-loading ()
  (remove-element *loading-banner*)
  (remove-element (js-get-element-by-id "loadBanner"))
  nil)

(defun-f my-boot ()
  (if (not *app*)
      (progn
        (show-loading)
        (prevent-page-close)
        (disable-back-button)
        (register-hash-cb "#devel"
          (lambda ()
            (if (not (equal (get-my-version) "devel"))
                (progn
                  (setf (jscl::oget (jscl::%js-vref "document") "cookie")
                        (format nil "~A=devel" (get-omg-cookie-name)))
                  (allow-page-close)
                  ((jscl::oget (jscl::%js-vref "location") "reload") t)))))
        (ensure-last-version)
        (setup-indexed-db (db "Tomo")
          (make-instance 'idb-object-store :connection db :name "sources"))
        (setf *app* (make-instance 'app))
        (append-element (render-widget *app*))
        (hide-loading))))

(defun-f show-commit-notify (cookie-name version)
  (show-notification
    (create-element "div" :|innerHTML| "New version available!"
                          :|style.color| "red")
    (create-element "div"
      :append-element "Save you work and click "
      :append-element (create-element "a"
                        :|href| "#"
                        :|onclick|
                          (lambda (ev)
                            (setf (jscl::oget (jscl::%js-vref "document") "cookie")
                                  (format nil "~A=~A" cookie-name version))
                            (allow-page-close)
                            ((jscl::oget (jscl::%js-vref "location") "reload") t)
                            nil)
                        :append-element "here")
      :append-element " to proceed to new version.")
    :period 5)
  nil)

(defun commit-notify (version)
  (remote-exec `(show-commit-notify ,+omg-version-cookie+ ,version))
  nil)

(defvar old-init #'omg-init)

(defun omg-init (port)
  (setf omg::*use-wss* t)
  (setf *debugger-hook* (lambda (&rest args) (declare (ignore args))))
  (funcall old-init port))

(set-boot '(my-boot))

(set-root-html "<span id='loadBanner'>TomoV is loading, please wait...</span>")
