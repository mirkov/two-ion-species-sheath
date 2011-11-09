;; Mirko Vukovic
;; Time-stamp: <2011-11-08 16:04:33 example-plot.lisp>
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

(in-package :2issu)

(defun POP11-Fig6 (&key (kinetic t) (approx nil))
  (let* ((n1 (lseq 1e-3 0.999d0 51))
	 (n2 (gsmap #'(lambda (x) (- 1d0 x)) n1)))
    (let ((M1 40)
	  (M2 131.29)
	  (Te 0.7)
	  (Ti 0.04))
      (grid-bind (V1 V2)
	  (gpmap (V1/2 @!n1 @!n2 M1 M2 Te Ti :kinetic kinetic
		       :approx approx) n1 n2)
	(set-to ((xlabel "n_{Ar+}/n_e")
		 (ylabel "m/s")
		 (title "Ion flow speeds in an Ar+/Xe+ plasma (POP11, Fig6)"))
	  (plot-xys n1 `((,V1)
			 (,V2))))))))