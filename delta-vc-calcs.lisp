;; Mirko Vukovic
;; Time-stamp: <2011-11-06 20:40:55 delta-vc-calcs.lisp>
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

(defun V1/2 (n1 n2 M1 M2 Te Ti &key kinetic (approx t) trace)
  "Calculate sheath velocities for the two species"
  (when trace
    (format t "n1: ~15t~5,3f~%" n1)
    (format t "n2: ~15t~5,3f~%" n2)
    (format t "M1: ~15t~5,3f~%" M1)
    (format t "M2: ~15t~5,3f~%" M2)
    (format t "Te: ~15t~5,3f~%" Te)
    (format t "Ti: ~15t~5,3f~%" Ti))
  (let ((vt1 (vti Ti M1))
	(vt2 (vti Ti M2))
	(alpha (alpha n1 m1 n2 m2))
	(cs1 (vti Te M1))
	(cs2 (vti Te M2)))
    (when trace
      (format t "vt1: ~15t~5,3e~%" vt1)
      (format t "vt2: ~15t~5,3e~%" vt2)
      (format t "alpha: ~15t~5,3e~%" alpha)
      (format t "cs1: ~15t~5,3e~%" cs1)
      (format t "cs2: ~15t~5,3e~%" cs2))
    (let ((Delta-V-proposed (if kinetic (Delta-Vc-kin n1 vt1 ti n2 vt2 ti)
				(Delta-Vc-fluid alpha vt1 vt2))))
      (when trace
	  (format t "Delta-V-proposed: ~15t~5,3e~%" Delta-V-proposed))
      (let ((Delta-V (Delta-Vc cs1 cs2 Delta-V-proposed))
	    (cs (cs n1 cs1 n2 cs2)))
	(when trace
	  (format t "Delta-V: ~15t~5,3e~%" Delta-V)
	  (format t "cs: ~15t~5,3e~%" cs))
	(let* ((V1 (if approx (V1-inst cs n1 n2 cs2 Delta-V)
		       (V1-exact n1 n2 cs1 cs2 Delta-V)))
	       (V2 (- V1 Delta-V)))
	  (when trace
	    (format t "V1: ~15t~5,3e~%" V1)
	    (format t "V2: ~15t~5,3e~%" V2))
	  (list V1 V2))))))

