;; Mirko Vukovic
;; Time-stamp: <2011-11-05 12:46:32 model-equations.lisp>
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

POP11 (54)"
  (let ((Delta-cs (- cs1 cs2)))
  (if (<= Delta-Vc-proposed (abs Delta-cs))
      Delta-Vc-proposed
      Delta-cs)))

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
  (defun V1-exact (n1 n2 cs1 cs2 Delta-Vc &optional print-steps)
    (setf n1% n1
	  n2% n2
	  cs1% cs1
	  cs2% cs2
	  Delta-Vc% Delta-Vc)
    (let* ((max-iter 50)
	   (cs (cs n1 cs1 n2 cs2))
	   (v1-approx (v1-inst cs n1 n2 cs2 Delta-Vc))
	   (solver
	    (make-one-dimensional-root-solver-f +brent-fsolver+
						     'V1-equation% (* 0.1d0 v1-approx)
						(* 10d0 v1-approx))))
      (when print-steps
	(format t "iter ~6t   [lower ~24tupper] ~36troot ~44terr ~54terr(est)~&"))
      (loop for iter from 0
	 for root = (solution solver)
	 for lower = (fsolver-lower solver)
	 for upper = (fsolver-upper solver)
	 do (iterate solver)
	 while  (and (< iter max-iter)
		     (not (root-test-interval lower upper 0.0d0 0.001d0)))
	 do
	 (when print-steps
	   (format t "~d~6t~10,6f~18t~10,6f~28t~12,9f ~44t~10,4g ~10,4g~&"
		   iter lower upper
		   root (- root (sqrt 5.0d0))
		   (- upper lower)))
	 finally (return root)))))