(in-package :cl-user)

(ql:quickload :local-time)
(ql:quickload :cl-html5-parser)

(defpackage :cl-chrome-bookmarks
  (:use :cl
	:local-time
        :html5-parser)
  (:export :parse-bookmarks-from-html
	   :list-bookmark-items))

(in-package :cl-chrome-bookmarks)

(defclass bookmark ()
  ((title
    :accessor title
    :initarg :title
    :documentation "bookmark title")
   (url
    :accessor url
    :initarg :url
    :documentation "bookmark URL")
   (icon
    :accessor icon
    :initarg :icon
    :documentation "bookmark icon")
   (add-date
    :accessor add-date
    :initarg :add-date
    :documentation "bookmark add date")))

(defvar *bookmark-items* ()
  "bookmarks instances list")

(defun list-bookmark-items ()
  "list bookmarks with title ,url and add-date attributes"
  (dolist (item *bookmark-items*)
    (format t "Title: ~A~%URL: ~A~%Date: ~A~%~%"
	    (title item)
	    (url item)
	    (add-date item))))

(defun parse-bookmarks-from-html (html-path)
  "parse exported chrome bookmarks html file"
  (let* ((content (uiop:read-file-string (pathname html-path)))
	 (body (parse-html5 content)))
    (traverse-nodes #'standard-recurse-p #'convert-to-bookmark body)))

(defun traverse-nodes (recurse-p fn node)
  "fn is applied to each visited node
   recurse-p controls whether to visit the children of node"
  (if node
      (progn
	(funcall fn node)
	(if (funcall recurse-p node)
	    (element-map-children
	     (lambda (n-node)
	       (traverse-nodes recurse-p fn n-node))
	     node)))))

(defun convert-to-bookmark (node)
  "visit node and convert attributes to make bookmark instance"
  (when (equal (node-name node) "a")
    (let ((title (node-value
		  (node-first-child node)))
	  (url (element-attribute node "href"))
	  (icon (element-attribute node "icon"))
	  (add-date (element-attribute node "add_date")))
      (format t "~A~%" title)
      (push (make-instance 'bookmark
			   :title title
			   :url url
			   :icon icon
			   :add-date (local-time:unix-to-timestamp
				      (parse-integer add-date)))
	    *bookmark-items*))))

(defun standard-recurse-p (node)
  "returns true only if you aren't trying to recurse into a script,
  style, or noscript tag."
  (not (or (equalp (node-name node) "script")
	   (equalp (node-name node) "style")
	   (equalp (node-name node) "noscript"))))
