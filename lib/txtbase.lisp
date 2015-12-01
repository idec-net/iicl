(defpackage :txt-base
  (:use :common-lisp)
  (:export :get-echoarea
	   :get-message))

(in-package :txt-base)

;;;
;;; Check and create base directories
;;;
(ensure-directories-exist "echo/")
(ensure-directories-exist "msg/")

(defun get-echoarea (echoarea)
  (let (lines)
    (with-open-file (in (concatenate 'string "echo/" echoarea)
			:if-does-not-exist :error)
      (loop for line = (read-line in nil)
	 while line do
	   (push line lines)))
    (reverse lines)))

(defun get-message (msgid)
  (let (lines)
    (with-open-file (in (concatenate 'string "msg/" msgid)
			:if-does-not-exist :error)
      (loop for line = (read-line in nil)
	 while line do
	   (push line lines)))
    (if lines
	(reverse lines)
	nil)))
