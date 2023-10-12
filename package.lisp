;;;; package define

(defpackage #:cl-parse-bookmarks
  (:use
   :common-lisp
   :local-time
   :html5-parser)
  (:export
   #:parse-bookmarks-from-html
   #:list-bookmark-items
   #:save-bookmark-items))
