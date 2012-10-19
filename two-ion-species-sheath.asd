;;;; two-ion-species-sheath.asd

(asdf:defsystem #:two-ion-species-sheath
  :serial t
  :components ((:file "two-ion-species-sheath-package-def")
               (:file "plasma-params")
	       (:file "model-equations")
	       (:file "Ar-Xe-exp-comparison")
	       (:file "delta-vc-calcs"))
  :depends-on (:alexandria
               :gsll
               :grid
	       :foreign-array
	       :mv-grid-utils
	       :my-utils
	       :lisp-unit
	       :mv-gnuplot))

