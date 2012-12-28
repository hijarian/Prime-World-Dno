;;; dno.lisp

(in-package #:dno)

(defparameter *ratings-xpath* 
  "//div[@class='b-ratinglist__i2']/div[contains(concat(' ', @class, ' '), ' b-ratings ')]")

(defparameter *class-names*
  (make-hash-table
   :test #'equal
   :size 100)
  "A hash defining mapping between hero class codes and hero class names")
(setf (gethash "duel" *class-names*) "Дуэлянт")
(setf (gethash "krio" *class-names*) "Крио")
(setf (gethash "faceless" *class-names*) "Безликий")
(setf (gethash "warloard" *class-names*) "Воевода")
(setf (gethash "molnienosnyi" *class-names*) "Молниеносный")
(setf (gethash "ten" *class-names*) "Тень")
(setf (gethash "eger" *class-names*) "Егерь")
(setf (gethash "izobretatel" *class-names*) "Изобретатель")
(setf (gethash "gorec" *class-names*) "Горец")
(setf (gethash "combat" *class-names*) "Комбат")
(setf (gethash "firefox" *class-names*) "Огненная лиса")
(setf (gethash "hiler" *class-names*) "Целительница")
(setf (gethash "night" *class-names*) "Царица ночи")
(setf (gethash "gora" *class-names*) "Человек-гора")
(setf (gethash "chist" *class-names*) "Чистильщик")
(setf (gethash "deva" *class-names*) "Дева")
(setf (gethash "strelok" *class-names*) "Стрелок")
(setf (gethash "znecdush" *class-names*) "Жнец душ")
(setf (gethash "krisolov" *class-names*) "Крысолов")
(setf (gethash "luchnica" *class-names*) "Лучница")
(setf (gethash "klik" *class-names*) "Клык")
(setf (gethash "zhaba" *class-names*) "Жабий наездник")
(setf (gethash "vedun" *class-names*) "Ведун")
(setf (gethash "charozmey" *class-names*) "Чарозмей")
(setf (gethash "muza" *class-names*) "Муза")
(setf (gethash "master" *class-names*) "Мастер клинков")
(setf (gethash "zaklinatel" *class-names*) "Заклинатель")
(setf (gethash "fiksy" *class-names*) "Фикси")
(setf (gethash "witcherb" *class-names*) "Ведьмак")
(setf (gethash "dokhtrina" *class-names*) "Доктрина")
(setf (gethash "prince-thief" *class-names*) "Принц воров")
(setf (gethash "viuga" *class-names*) "Вьюга")
(setf (gethash "white-mask" *class-names*) "Белая маска")
(setf (gethash "predvoditel-adore" *class-names*) "Предводитель")
(setf (gethash "gromoverjec" *class-names*) "Громовержец")
(setf (gethash "nevidimka" *class-names*) "Невидимка")
(setf (gethash "tancuyuschii-s-volkami" *class-names*) "Танцующий с волками")
(setf (gethash "hudoshnica" *class-names*) "Художница")
(setf (gethash "bessmertny" *class-names*) "Бессмертный")
(setf (gethash "mejin" *class-names*) "Мейдзин")
(setf (gethash "redhvost" *class-names*) "Рыжий Хвост")
(setf (gethash "grica" *class-names*) "Жрица")
(setf (gethash "pantera" *class-names*) "Чёрная пантера")
(setf (gethash "rokot" *class-names*) "Рокот")
(setf (gethash "asasin" *class-names*) "Ассасин")
(setf (gethash "nimfa" *class-names*) "Нимфа")
(setf (gethash "ohotnik" *class-names*) "Охотник")
(setf (gethash "dushelov" *class-names*) "Душелов")
(setf (gethash "povelitelkris" *class-names*) "Повелитель крыс")
(setf (gethash "amazonka" *class-names*) "Амазонка")
(setf (gethash "kogot" *class-names*) "Коготь")
(setf (gethash "bolotcaras" *class-names*) "Болотный Царь")
(setf (gethash "lesovik" *class-names*) "Лесовик")
(setf (gethash "magozavr" *class-names*) "Магозавр")
(setf (gethash "bard" *class-names*) "Бард")
(setf (gethash "otmechennij" *class-names*) "Отмеченный Змеем")
(setf (gethash "charodey" *class-names*) "Чародей")
(setf (gethash "koroleva_fey" *class-names*) "Королева фей")
(setf (gethash "witchera" *class-names*) "Ведьмак")
(setf (gethash "demonolog" *class-names*) "Демонолог")

(defun make-human-readable-hero-class (hero-class)
  (or (gethash hero-class *class-names*) hero-class))

(defun get-hero-class (subnode)
  (string-trim " "
               (cl-ppcre:regex-replace-all "b-ratings-?" 
                                           (xtree:attribute-value subnode "class") 
                                           "")))

(defun get-rating-for-player (subnode)
  "Accepts the DOM element with data about single player standing, player name and rating"
  (list 
   (cons "name" (xpath:find-string subnode "./em[contains(concat(' ', @class, ' '), ' nickname ')]"))
   (cons "x"    (parse-integer (xpath:find-string subnode "./b")))
   (cons "y"    (parse-integer (remove-if-not #'alphanumericp (xpath:find-string subnode "./span"))))))

(defun get-field-from-point (field-name point-definition)
  (cdr (assoc field-name point-definition))) 

(defun get-y-field-from-point (point-definition)
  (get-field-from-point "y" point-definition))

(defun list-average (data)
  "Calculate average value for a list of numbers"
  (let ((elements-count (length data))
        (elements-sum (reduce #'+ data)))
    (coerce (/ elements-sum elements-count) 'float)))
  
(defun calculate-mean (data)
  "Calculate average between the every y field from the given alist"
  (let ((elements (mapcar #'get-y-field-from-point data)))
    (list-average elements)))

(defun calculate-mean-for-last (items-number data)
  (calculate-mean (last data items-number)))

(defun get-ratings-for-hero (node)
  "Accepts the DOM element with data about all ratings for a given hero"
  (let ((hero-class (get-hero-class node))
        (data-items (map-on-node-by-xpath node "./ul/li" #'get-rating-for-player)))
    (if (not (or (equal hero-class "h-common") (equal hero-class "h-doct") (equal hero-class "h-adorn")))
        (list
         (cons "name" (make-human-readable-hero-class hero-class))
         (cons "data" data-items)
         (cons "mean" (calculate-mean data-items))
         (cons "mean-last" (calculate-mean-for-last 20 data-items))))))
    
(defun collect-ratings (html-page)
  "From the raw HTML text gets ratings list"
  (remove-if #'null (map-on-xpath html-page *ratings-xpath* #'get-ratings-for-hero)))


;; MAIN 
(defun get-ratings-page ()
  (drakma:http-request "http://ru.playpw.com/ratings.html"))

(defun ratings-to-json (ratings)
  (json:encode-json-to-string ratings))

(defun send-data-from-origin-as-json ()
  (let ((page (get-ratings-page)))
    (ratings-to-json (collect-ratings page))))


;;; WRITING TO FILE
(defun write-ratings-str-to-file (ratings-str outfile)
  (with-open-file (output (merge-pathnames outfile) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line ratings-str output)))

(defun write-ratings-to-file (outfile)
  "Auxiliary function to debug"
  (let ((page (get-ratings-page)))
    (write-ratings-str-to-file 
     (ratings-to-json (collect-ratings page))
     outfile)))








