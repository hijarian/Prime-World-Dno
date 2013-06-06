; helpers.lisp
; Various helper functions to use through the codebase
(in-package :dno)

;-------------------------------------------------------------------------------
; Hunchentoot

(defun publish-directory (uri dirname)
  "Makes files in given <dirname> accessible under the URL prefix <uri>. <dirname> should be in relative directory path format, e. g. \"foo/bar/baz\""
  (push (hunchentoot:create-folder-dispatcher-and-handler uri dirname)
        hunchentoot:*dispatch-table*))

(defun publish-file (uri filename)
  (push (hunchentoot:create-static-file-dispatcher-and-handler uri filename)
        hunchentoot:*dispatch-table*))

(defmacro publish-ajax-endpoint (uri name params &body body)
  "<name> is a name of handler function, <uri> is a relative URI for endpoint, <params> is a list of symbol names of expected params in request>"
  `(hunchentoot:define-easy-handler (,name :uri ,uri) ,params 
     (setf (hunchentoot:content-type*) "application/json")
     ,@body))

;-------------------------------------------------------------------------------
; XPath

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

