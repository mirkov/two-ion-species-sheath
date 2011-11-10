;; Mirko Vukovic
;; Time-stamp: <2011-11-10 09:47:19 model-equations.lisp>
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

(defun alpha (n1 M1 n2 M2)
  "Parameter alpha

POP11 (29)"
  (/ (* n1 M2)
     (* n2 M1)))

(defun cs (n1 cs1 n2 cs2)
  "System sound speed

POP11, below (56)"
  (sqrt (/ (+ (* n1 (^2 cs1))
	      (* n2 (^2 cs2)))
	   (+ n1 n2))))

(defun Delta-Vc-fluid (alpha vt1 vt2)
  "Delta-Vc fluid

POP11 (40)"
  (* (sqrt (/ (+ 1 alpha)
	      (* 2 alpha)))
     (sqrt (+ (^2 vt1)
	      (^2 vt2)))))

(defun Delta-Vc-kin (n1 vt1 T1 n2 vt2 T2)
  "Delta-Vc kinetic

POP11 (53)"
  (+ (* -3/2
	(abs (- vt1 vt2)))
     (sqrt (* 0.5
	      (+ 1 (* (/ n2 n1)
		      (/ T1 T2)))
	      (+ (^2 vt1)
		 (* (/ n1 n2)
		    (/ T2 T1)
		    (^2 vt2)))))))

(defun Delta-Vc (cs1 cs2 Delta-Vc-proposed)
  "Actual Delta-Vc, depending on relative magnitude of the arguments

In addition, return whether the Delta-Vc-proposed was accepted

POP11 (54)"
  (let* ((Delta-cs (- cs1 cs2))
	 (test (<= Delta-Vc-proposed (abs Delta-cs))))
    (values (if test
		Delta-Vc-proposed
		Delta-cs)
	    test)))

(defun V1-inst (cs n1 n2 cs2 Delta-Vc)
  "Sheath velocity of species 1, when instabilities are present

POP11 (58)"
  (+ cs (* (/ n2 (+ n1 n2))
	   (^2 (/ cs2 cs))
	   Delta-Vc)))


(defun V2-inst (cs n1 n2 cs1 Delta-Vc)
  "Sheath velocity of species 1, when instabilities are present

POP11 (58)"
  (- cs (* (/ n1 (+ n1 n2))
	   (^2 (/ cs1 cs))
	   Delta-Vc)))


#|
(map-grid :source #(-120 274 -225 85 -15 1.0) :destination-specification 
		'((foreign-array 6) double-float))
|#

(defun solve-V1-poly (n1 n2 cs1 cs2 delta)
  "Solve fourth order polynomial for species 1 Bohm Velocity"
  (let ((ne (+ n1 n2)))
    (let ((n1/ne (/ n1 ne))
	  (cs1^2 (expt cs1 2))
	  (Delta^2 (expt Delta 2)))
      (let ((cs^2 (expt (cs n1 cs1 n2 cs2) 2)))
	(let ((c0 (- (* n1/ne cs1^2 Delta^2)))
	      (c1 (* 2d0 Delta n1/ne cs1^2))
	      (c2 (- Delta^2 cs^2))
	      (c3 (- (* 2d0 Delta)))
	      (c4 1d0))
	  (let ((coeffs (make-array 5 :initial-contents
				    (list c0 c1 c2 c3 c4))))
	    (let ((roots
		   (coerce
		    (grid:map-grid
		     :source
		     (gsll:polynomial-solve
		      (grid:map-grid :source coeffs
				     :destination-specification
				     '((grid::foreign-array 5) double-float)))
		     :destination-specification '((array 4) double-float))
		    'list)))
	      (assert (= 2 (count-if #'zerop roots :key #'imagpart))
		      ()
		      "Did not find two real roots ~a" roots)
	      (realpart (find-if #'(lambda (root)
				     (when (zerop (imagpart root))
				       (< 0 (realpart root))))
				 roots)))))))))
	  

(let (n1% n2% cs1% cs2% Delta-Vc%)
  (defun V1-equation% (V1%)
    "Quartic equation for V1

POP11 (55)"
    (+ (* (/ n1% (+ n1% n2%))
	  (/ (^2 cs1%)
	     (^2 V1%)))
       (* (/ n2% (+ n1% n2%))
	  (/ (^2 cs2%)
	     (^2 (- V1% Delta-Vc%))))
       -1d0))
  (defun V1-exact (n1 n2 cs1 cs2 Delta-Vc)
    (solve-V1-Poly n1 n2 cs1 cs2 Delta-Vc)))