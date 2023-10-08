(in-package :cl-user)

(ql:quickload :local-time)
(ql:quickload :cl-html5-parser)

(defpackage cl-chrome-bookmarks
  (:use :cl
        :uiop)
  (:export :list-bookmarks
           :parse-bookmarks-from-html))

(in-package :cl-chrome-bookmarks)


(defvar *bookmarks-db* nil)

(defclass bookmark ()
  ((title
    :accessor title
    :initarg :title
    :documentation "Bookmark Title")
   (url
    :accessor url
    :initarg :url
    :documentation "Bookmark URL")
   (icon
    :accessor icon
    :initarg :icon
    :documentation "Bookmark Icon")
   (add-date
    :accessor add-date
    :initarg :add-date
    :documentation "Bookmark Add Date")))

(defmethod list-bookmarks ()
  (dolist (bookmark *bookmarks-db*)
    (format t
            "Title: ~A~%URL: ~A~%Icon: ~A~%Date: ~A~%~%"
            (title bookmark) (url bookmark) (icon bookmark) (add-date bookmark))))

(defun parse-bookmarks-from-html (path)
  (let* ((content (read-file-string (pathname path)))
         (body (html5-parser:parse-html5 content :dom :xmls-ns))
         (node (nth 4 (nth 3 body))))
    (find-dom-tag node #'(lambda (node)
                           (push (make-instance 'bookmark
                                                :title (nth 2 node)
                                                :url (nth 1 (nth 0 (nth 1 node)))
                                                :icon (nth 1 (nth 2 (nth 1 node)))
                                                :add-date (local-time:unix-to-timestamp
                                                           (parse-integer
                                                            (nth 1 (nth 1 (nth 1 node))))))
                                 *bookmarks-db*))
                  "a")))

(defun find-dom-tag (node fn tag)
  (unless (null node)
    (when (listp node)
      (find-dom-tag (car node) fn tag)
      (find-dom-tag (cdr node) fn tag)
      (when (equal (car node) tag)
        (funcall fn node)))))
