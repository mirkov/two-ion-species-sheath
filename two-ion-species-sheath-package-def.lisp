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
  (:export :V1/V2-ion-sound-speed
	   :V1/V2-system-sound-speed
	   :V1/V2-ion-ion-instability
	   :V1/V2
	   :*trace-p* :*kinetic-p* :*approx-p* :*model*)
  (:documentation 
"Implementation of models for ion sheath velocity

It provides model for the system sound speed, individual sound speeds and Baalrud & Hegna's theory for ion-ion instability effects
"))



