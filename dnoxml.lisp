(ql:quickload '(:iterate :cl-libxml2 :drakma :cl-ppcre :cl-json))

;;------------------------------------------------------------------------------
;; UTILITY FUNCTIONS BEGIN
(defun map-on-node-by-xpath (node xpath functor)
  "Apply functor to every node gotten by xpath on the node given"
  (iterate:iter (iterate:for subnode 
                             in-xpath-result 
                             xpath
                             on node)
                (iterate:collect (funcall functor subnode))))

(defun map-on-xpath (html xpath functor)
  "Map functor onto all nodes matched by xpath in given html text"
  (html:with-parse-html (document html)
    (map-on-node-by-xpath document xpath functor)))

;; UTILITY FUNCTIONS END
;;------------------------------------------------------------------------------


(defparameter *ratings-xpath* 
  "//div[@class='b-ratinglist__i2']/div[contains(concat(' ', @class, ' '), ' b-ratings ')]")

(defun get-hero-class (subnode)
  (string-trim " "
               (cl-ppcre:regex-replace-all "b-ratings-?" 
                                           (xtree:attribute-value subnode "class") 
                                           "")))

(defun get-rating-for-player (subnode)
  "Accepts the DOM element with data about single player standing, player name and rating"
  (list 
   (xpath:find-string subnode "./b")
   (xpath:find-string subnode "./em")
   (xpath:find-string subnode "./span")))

(defun get-ratings-for-hero (node)
  "Accepts the DOM element with data about all ratings for a given hero"
  (cons (get-hero-class node) 
        (map-on-node-by-xpath node "./ul/li" #'get-rating-for-player)))

(defun collect-ratings (html-page)
  "From the raw HTML text gets ratings list"
  (map-on-xpath html-page *ratings-xpath* #'get-ratings-for-hero))

(defun write-ratings-str-to-file (ratings-str outfile)
  (with-open-file (output (merge-pathnames outfile) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line ratings-str output)))


;; MAIN 
(defun get-ratings-page ()
  (drakma:http-request "http://ru.playpw.com/ratings.html"))

(defun ratings-to-json (ratings)
  (json:encode-json-to-string ratings))

(defun run ()
  "Main entry point"
  (let ((page (get-ratings-page))
        (outfile "ratings.json"))
    (write-ratings-str-to-file 
     (ratings-to-json (collect-ratings page))
     outfile)))








