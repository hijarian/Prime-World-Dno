;;;; init.lisp

(in-package :dno)

(hunchentoot:define-easy-handler (index :uri "/") ()
  (setf (cl-who:html-mode) :HTML5)
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title "Кто дно?")
      (:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js")
      (:script :src "http://code.highcharts.com/highcharts.js")
      (:script :src "http://code.highcharts.com/modules/exporting.js")
      (:link :rel "stylesheet" :href "/assets/styles.css"))
     (:body
      (:div :id "container"
            (:img :id "loading-icon" :src "/assets/loading.gif"))
            (:h1 :id "loading-header" "Загрузка данных...")
      (:script :src "/assets/scripts.js")))))

(hunchentoot:define-easy-handler (data :uri "/data") ()
  (setf (hunchentoot:content-type*) "application/json")
  (send-data-from-origin-as-json))

(publish-directory "/assets/" "assets/")

;;; Now the pain in the ass: we cannot just do (publish-directory "/" "webroot"),
;;; because it will break webapp root URL completely (and published files will not be accessible, too)
;;; So, we will publish then one-by-one.

(publish-webroot-file-list 
 "webroot"
 '("favicon.ico" 
   "robots.txt" 
   "crossdomain.xml"))

(defun run ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))
