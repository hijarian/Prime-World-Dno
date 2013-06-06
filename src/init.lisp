; init.lisp
; Here we setup routes in our webapp and define a only public function which starts the web server.
(in-package :dno)

;; Webroot
(publish-file "/" "webroot/index.html")

;; All CSS and JS files referenced from webroot file
(publish-directory "/assets/" "assets/")

;; Now the pain in the ass: we cannot just do (publish-directory "/" "webroot"),
;; because it will break webapp root URL completely (and published files will not be accessible, too)
;; So, we will publish then one-by-one.
(publish-webroot-file-list "webroot" '("favicon.ico" "robots.txt" "crossdomain.xml")) 

;; AJAX endpoint to grab data to display in chart
(hunchentoot:define-easy-handler (data :uri "/data") ()
  (setf (hunchentoot:content-type*) "application/json")
  (send-data-from-origin-as-json))

(defun run ()
  "Launch Hunchentoot web server instance on default port"
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))
