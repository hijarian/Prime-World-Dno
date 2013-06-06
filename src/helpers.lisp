; helpers.lisp
; Various helper functions to use through the codebase
(in-package :dno)

;;; HUNCHENTOOT ASSETS PUBLISHING HELPERS BEGIN
(defun publish-directory (uri dirname)
  "Makes files in given <dirname> accessible under the URL prefix <uri>. <dirname> should be in relative directory path format, e. g. \"foo/bar/baz\""
  (push (hunchentoot:create-folder-dispatcher-and-handler uri dirname)
        hunchentoot:*dispatch-table*))

(defun publish-file (uri filename)
  (push (hunchentoot:create-static-file-dispatcher-and-handler uri filename)
        hunchentoot:*dispatch-table*))

(defun publish-webroot-file (webroot-dirname webroot-filename)
  "Makes file under the <webroot-dirname> directory in the root application directory accessible under the URI in root of web-app."
  (push (hunchentoot:create-static-file-dispatcher-and-handler 
         (format NIL "/~A" webroot-filename)
         (format NIL "~A/~A" webroot-dirname webroot-filename))
        hunchentoot:*dispatch-table*))

(defun publish-webroot-file-list (webroot-dirname webroot-files)
  (loop 
     for webroot-filename 
     in webroot-files
     doing (publish-webroot-file webroot-dirname webroot-filename)))
;;; HUNCHENTOOT ASSETS PUBLISHING HELPERS END

;;; XPATH HELPERS BEGIN
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
;;; XPATH HELPERS END

