(defpackage :ii-filters
  (:use :common-lisp
	:cl-ppcre)
  (:export :msg-filter))

(in-package :ii-filters)

(defun msg-filter (msgid)
  (scan "^[a-zA-Z0-9]{20}$" msgid))
