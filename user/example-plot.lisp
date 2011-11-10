;; Mirko Vukovic
;; Time-stamp: <2011-11-09 16:49:46 example-plot.lisp>
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

(defun plot-vb-vs-XI-1 (M1 M2 Te Ti &key (kinetic t) (approx nil))
  "Plot of Bohm velocities of species 1&2"
  (let* ((n1 (lseq 1e-3 0.999d0 201))
	 (n2 (gsmap #'(lambda (x) (- 1d0 x)) n1)))
    (grid-bind (V1 V2)
	(gpmap (V1/2 @!n1 @!n2 M1 M2 Te Ti :kinetic kinetic
		     :approx approx) n1 n2)
      (plot-xys n1 `((,V1)
		     (,V2))))))

(defun POP11-Fig6 ()
  (set-to ((xlabel "n_{Ar+}/n_e")
	   (ylabel "cm/s")
	   (title "Ion flow speeds in an Ar+/Xe+ plasma (POP11, Fig6)"))
    (plot-vb-vs-Xi-1 40 132.3 .7 .04)))