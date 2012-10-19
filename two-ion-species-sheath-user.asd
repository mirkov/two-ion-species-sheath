(asdf:defsystem :two-ion-species-sheath-user
  :serial t
  :components ((:file "two-ion-species-sheath-user-package")
	       (:file "user/example-plot"))
  :depends-on (:two-ion-species-sheath
	       :lisp-unit
	       :mv-grid-utils
	       :mv-gnuplot))