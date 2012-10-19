;; Mirko Vukovic
;; Time-stamp: <2011-12-18 17:07:13 delta-vc-calcs.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :2ISS)

(defun V1/V2-ion-sound-speed (M1 M2 Te)
  "Return a two element list of individual ion sound speeds"
  (let ((cs1 (vti Te M1))
	(cs2 (vti Te M2)))
    (list cs1 cs2)))

(defun V1/V2-system-sound-speed (M1 M2 Te N1 N2)
  "Return a two element list of system ion sound speed"
  (let* ((cs1 (vti Te M1))
	 (cs2 (vti Te M2))
	 (cs (cs n1 cs1 n2 cs2)))
    (list cs cs)))

(defparameter *trace-p* nil
  "Turns on tracing facilities")
(defparameter *kinetic-p* t
  "Turns on the kinetic model")
(defparameter *approx-p* nil
  "Turns on the approximate solution")


(defun V1/V2-ion-ion-instability (M1 M2 Te N1 N2 Ti)
  "Calculate sheath velocities for the two species using the ion-ion instability model.  The ion species are specified by:
- their relative densities n1 & n2 whose sum should equal unity
- masses (in AMU) with M1 < M2
- Electron and ion temperature in eV

Returns a two element list of ions velocities and two other values:
system sound speed, and whether the correction due to instabilities
was applied (POP 54)

Three flags/special variables control the calculation of the instability
effects:
- *kinetic-p* turn on the kinetic calculation of the instability effects
- *approx-p* turns on the approximate solution for the bohm velocities
- *trace-p* turns on printiout of trace information "
  (let* ((alpha (alpha n1 m1 n2 m2))
	 (cs1 (vti Te M1))
	 (cs2 (vti Te M2))
	 (cs (cs n1 cs1 n2 cs2)))
    (when *trace-p*
      (format t "alpha: ~15t~5,3e~%" alpha)
      (format t "cs1: ~15t~5,3e~%" cs1)
      (format t "cs2: ~15t~5,3e~%" cs2)
      (format t "cs: ~15t~5,3e~%" cs))
    (let* ((vt1 (vti Ti M1))
	   (vt2 (vti Ti M2))
	   (Delta-V-proposed (if *kinetic-p* (Delta-Vc-kin n1 vt1 ti n2 vt2 ti)
				 (Delta-Vc-fluid alpha vt1 vt2))))
      (when *trace-p*
	(format t "vt1: ~15t~5,3e~%" vt1)
	(format t "vt2: ~15t~5,3e~%" vt2)
	(format t "Delta-V-proposed: ~15t~5,3e~%" Delta-V-proposed))
      (multiple-value-bind (Delta-V corrected-p)
	  (Delta-Vc cs1 cs2 Delta-V-proposed)
	(when *trace-p*
	  (format t "Delta-V: ~15t~5,3e~%" Delta-V))
	(let* ((V1 (if *approx-p* (V1-inst cs n1 n2 cs2 Delta-V)
		       (V1-exact n1 n2 cs1 cs2 Delta-V)))
	       (V2 (- V1 Delta-V)))
	  (when *trace-p*
	    (format t "V1: ~15t~5,3e~%" V1)
	    (format t "V2: ~15t~5,3e~%" V2))
	  (values (list V1 V2)
		  cs corrected-p))))))

(defgeneric V1/V2% (model M1 M2 Te &optional n1 n2 Ti)
  (:documentation "Calculate the sheath velocities for two ions of
mass M1 and M2, and for electron temperature Te

`model' specifies whether we use the :individual-sound-speed,
the :system-sound-speed or the :ion-ion-instability model

In case of the `ion-ion-instability' model, we need the values of the
densities `n1', `n2' and `Ti'

The methods are wrappers for the previously defined functions that
perform the calculations.  The primary purpose for the generic
function and the methods is to present a unified interface to V1/2
that uses the *model* special variable to determine which model is
used to calculate the ion sheath velocities ")
  (:method ((model (eql :individual-sound-speed)) M1 M2 Te
	    &optional n1 n2 Ti)
    (declare (ignore n1 n2 Ti))
    (v1/v2-ion-sound-speed M1 M2 Te))
  (:method ((model (eql :system-sound-speed)) M1 M2 Te
	    &optional n1 n2 Ti)
    (declare (ignore Ti))
    (v1/v2-system-sound-speed M1 M2 Te N1 N2))
  (:method ((model (eql :ion-ion-instability)) M1 M2 Te
	    &optional n1 n2 Ti)
    (v1/v2-ion-ion-instability M1 M2 Te N1 N2 Ti)))


(defparameter *model* nil
  "Model of ion-ion sheath velocity.  It can take one of three values:
 - :individual-sound-speed
 - :system-sound-speed
 - :ion-ion-instability")


(defun V1/V2 (M1 M2 Te &optional n1 n2 Ti)
  "Return a two element list of the sheath velocities for ions of mass
M1 & M2 and electron temperature Te

Depending on model, one may need to specify the ion densities `n1' and
`n2' and the ion temperature `Ti'"
  (v1/2% *model* M1 M2 Te n1 n2 Ti))

(defun normalize-0-1 (y y1 y2)
  "Calculate (y-y1)/(y2-y1)
 
When y1 <= y <= y2 is satisfied the returned value is between 0 and 1

The following must be satisfied: y1 <= y <= y2"
  (assert (<= y1 y y2) ()
	  "y1, y, and y2 must be in monotonically nondecreasing order")
  (/ (- y y1)
     (- y2 y1)))