;;;; init.lisp

(in-package :dno)

(defun create-index-page ()
  (setf (cl-who:html-mode) :HTML5)
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title "Кто дно?")
      (:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js")
      (:script :src "http://code.highcharts.com/highcharts.js")
      (:script :src "http://code.highcharts.com/modules/exporting.js")
      (:script :src "http://ajax.aspnetcdn.com/ajax/jquery.dataTables/1.9.4/jquery.dataTables.min.js")
      (:link :rel "stylesheet" :href "http://ajax.aspnetcdn.com/ajax/jquery.dataTables/1.9.4/css/jquery.dataTables.css")
      (:link :rel "stylesheet" :href "/assets/styles.css"))
     (:body
      (:h1 "Кто дно?")
      (:p "Данная программа скачивает рейтинги игроков в Prime World со страницы http://ru.playpw.com/ratings.html и отображает их в виде графиков. Опираясь на высоту графика относительно остальных, теоретически, можно судить об эффективности данного персонажа. Таким образом, персонаж, чей график рейтинга ниже всех остальных &#8212; дно. :)")
      
      (:ul :class "pages-toggler-wrapper"
           (:li :class "pages-toggler" :data-target "container" (:button "Посмотреть графики"))
           (:li :class "pages-toggler" :data-target "meanvalues" (:button "Посмотреть средние значения рейтинга")))
      
      (:div :id "container" :class "page"
            (:img :id "loading-icon" :src "/assets/loading.gif")
            (:h1 :id "loading-header" "Загрузка данных..."))

      (:div :id "meanvalues" :class "page"
            (:table :id "meanvalues-list"
                    (:thead 
                     (:tr 
                      (:th "Имя персонажа")
                      (:th "Средний рейтинг")
                      (:th "Средний рейтинг без учёта первых 5 мест")))))

      (:script :src "/assets/scripts.js")))))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (create-index-page))

(hunchentoot:define-easy-handler (data :uri "/data") ()
  (setf (hunchentoot:content-type*) "application/json")
  (send-data-from-origin-as-json))

(publish-directory "/assets/" "assets/")

;;; Now the pain in the ass: we cannot just do (publish-directory "/" "webroot"),
;;; because it will break webapp root URL completely (and published files will not be accessible, too)
;;; So, we will publish then one-by-one.
(publish-webroot-file-list "webroot" '("favicon.ico" "robots.txt" "crossdomain.xml")) 

(defun run ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))
