;; Mirko Vukovic
;; Time-stamp: <2011-11-19 22:53:26 plasma-params.lisp>
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

(defun vti (Ti Mi)
  "Ion thermal velocity as function of temperature (eV) and mass (amu)
in units of m/s

NRL Plasma Formulary"
  (* 9.79e3 (sqrt (/ Ti Mi))))