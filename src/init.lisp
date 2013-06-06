; init.lisp
; Here we setup routes in our webapp and define a only public function which starts the web server.
(in-package :dno)

;; Webroot
(publish-file "/" "webroot/index.html")

;; Favicon, just because
(publish-file "/favicon.ico" "webroot/favicon.ico")

;; Images, CSS and JS files referenced from index.html
(publish-directory "/assets/" "assets/")

;; AJAX endpoint to grab data to display in chart
(publish-ajax-endpoint "/data" data () (send-data-from-origin-as-json))

(defun run ()
  "Launch Hunchentoot web server instance on default port"
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))
