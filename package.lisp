;;;; package.lisp

(defpackage #:two-ion-species-sheath
  (:nicknames :2iss)
  (:use #:cl :alexandria :gsll :lisp-unit :mv-grid :mv-gnuplot)
  (:import-from :my-utils
		:^2)
  (:shadow :alexandria :mean :standard-deviation :variance :median
	   :factorial)
  (:shadow :lisp-unit
	   :set-equal)
  (:export :V1/2))

(defpackage :two-ion-species-sheath-user
  (:nicknames :2issu)
  (:use :cl :two-ion-species-sheath :lisp-unit :mv-grid :mv-gnuplot))

