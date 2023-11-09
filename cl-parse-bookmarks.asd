;;;; Google Chrome bookmarks parser for Common Lisp

(asdf:defsystem #:cl-parse-bookmarks
  :name "cl-parse-bookmarks"
  :description "Parse Chrome bookmarks with Common Lisp."
  :author "Wimi Yuan <qmdx00@gmail.com>"
  :license "MIT"
  :depends-on (:cl-html5-parser :local-time)
  :components ((:file "package")
	       (:file "bookmarks" :depends-on ("package"))))


