;; Mirko Vukovic
;; Time-stamp: <2011-11-08 15:55:59 Ar-Xe-exp-comparison.lisp>
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

(in-package :2iss)

(defun sample-calc ()
  (let* ((n1 0.9e-1)
	 (n2 (- 1 n1)))
    (format t "n1: ~15t~5,3f~%" n1)
    (format t "n2: ~15t~5,3f~%" n2)
    (let ((M1 40)
	  (M2 132)
	  (Te 0.7)
	  (Ti 0.04))
      (format t "M1: ~15t~5,3f~%" M1)
      (format t "M2: ~15t~5,3f~%" M2)
      (format t "Te: ~15t~5,3f~%" Te)
      (format t "Ti: ~15t~5,3f~%" Ti)
      (let ((vt1 (vti Ti M1))
	    (vt2 (vti Ti M2))
	    (alpha (alpha n1 m1 n2 m2))
	    (cs1 (vti Te M1))
	    (cs2 (vti Te M2)))
	(format t "vt1: ~15t~5,3e~%" vt1)
	(format t "vt2: ~15t~5,3e~%" vt2)
	(format t "alpha: ~15t~5,3e~%" alpha)
	(format t "cs1: ~15t~5,3e~%" cs1)
	(format t "cs2: ~15t~5,3e~%" cs2)
	(let ((Delta-V-fl (Delta-Vc-fluid alpha vt1 vt2))
	      (Delta-V-kin (Delta-Vc-kin n1 vt1 ti n2 vt2 ti)))
	  (format t "Delta-V-fl: ~15t~5,3e~%" Delta-V-fl)
	  (format t "Delta-V-kin: ~15t~5,3e~%" Delta-V-kin)
	  (let ((Delta-V (Delta-Vc cs1 cs2 Delta-V-fl))
		(cs (cs n1 cs1 n2 cs2)))
	    (format t "Delta-V: ~15t~5,3e~%" Delta-V)
	    (format t "cs: ~15t~5,3e~%" cs)
	    (let ((V1 (V1-inst cs n1 n2 cs2 Delta-V))
		  (V2 (V2-inst cs n1 n2 cs1 Delta-V)))
	      (format t "V1: ~15t~5,3e~%" V1)
	      (format t "V2: ~15t~5,3e~%" V2))))))))

(defun POP11-Fig6 (&key (kinetic t))
  (let* ((n1 (lseq 1e-3 0.999d0 51))
	 (n2 (gsmap #'(lambda (x) (- 1d0 x)) n1)))
    (let ((M1 40)
	  (M2 131.29)
	  (Te 0.7)
	  (Ti 0.04))
      (let ((vt1 (vti Ti M1))
	    (vt2 (vti Ti M2))
	    (alpha (gpmap (alpha @!n1 m1 @!n2 m2) n1 n2))
	    (cs1 (vti Te M1))
	    (cs2 (vti Te M2)))
	(let ((Delta-V-proposed
	       (if kinetic (gpmap (Delta-Vc-kin @!n1 vt1 ti @!n2 vt2 ti)
				  n1 n2)
		   (gpmap (Delta-Vc-fluid @!alpha vt1 vt2)
			  alpha))))
	  (let ((Delta-V (gpmap (Delta-Vc cs1 cs2 @!Delta-V-proposed)
				Delta-V-proposed))
		(cs (gpmap (cs @!n1 cs1 @!n2 cs2)
			   n1 n2)))
	    (let* ((V1
		    (if kinetic
			(gpmap (v1-exact @!n1 @!n2 cs1 cs2 @!Delta-V)
			       n1 n2 Delta-V)
			(gpmap (V1-inst @!cs @!n1 @!n2 cs2 @!Delta-V)
			       cs n1 n2 Delta-V)))
		   (V2 (gpmap (- @!V1 @!Delta-V)
			      V1 Delta-V)))
	      (set-to ((xlabel "n_{Ar+}/n_e")
		       (ylabel "m/s")
		       (title "Ion flow speeds in an Ar+/Xe+ plasma (POP11, Fig6)"))
		(plot-xys n1 `((,cs :title "cs")
			       (,V1 :title "v_{Ar+}")
			       (,V2 :title "v_{Xe+}")))))))))))


