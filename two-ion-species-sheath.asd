;;;; two-ion-species-sheath.asd

(asdf:defsystem #:two-ion-species-sheath
  :serial t
  :depends-on (#:lisp-unit
               #:alexandria
               :gsll
	       #:mv-gnuplot
	       :my-utils
	       :mv-grid-utils)
  :components ((:file "package")
               (:file "plasma-params")
	       (:file "model-equations")
	       (:file "Ar-Xe-exp-comparison")))

