;;;; wo-splot.asd

(asdf:defsystem #:wo-splot
  :serial t
  :description "Describe wo-splot here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on ("vecto" "cl-ppcre" "cl-colors" "net-telent-date" "cl-svg")
  :components ((:file "package")
               (:file "wo-splot")))

