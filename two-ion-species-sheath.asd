;;;; two-ion-species-sheath.asd

(asdf:defsystem #:two-ion-species-sheath
  :serial t
  :components ((:file "package")
               (:file "plasma-params")
	       (:file "model-equations")
	       (:file "Ar-Xe-exp-comparison"))
  :depends-on (#:lisp-unit
               #:alexandria
               :gsll
	       #:mv-gnuplot
	       :my-utils
	       :mv-grid-utils))

(asdf:defsystem :two-ion-species-sheath-user
  :serial t
  :components ((:file "user/example-plot"))
  :depends-on (:two-ion-species-sheath
	       :lisp-unit
	       :mv-grid-utils
	       :mv-gnuplot))