(ql:quickload :split-sequence)
(ql:quickload :hunchentoot)
(ql:quickload :cl-base64)
(ql:quickload :flexi-streams)
(ql:quickload :cl-ppcre)

(load "lib/txtbase.lisp")
(load "lib/filters.lisp")

(defpackage :iicl
  (:use :common-lisp
	:split-sequence
	:hunchentoot
	:cl-base64
	:flexi-streams
	:cl-ppcre
	:txt-base
	:ii-filters)
  (:export :load-config
	   :sublist))

(in-package :iicl)

(defvar *nodename* "")
(defvar *echoareas* ())

(defun load-config ()
  "Load data from the configuration file."
  (setf *echoareas* nil)
  (with-open-file (in "iicl.cfg")
    (loop for line = (read-line in nil)
       while line do
	 (let ((param (split-sequence #\space line)))
	   (if (>= (length param) 2)
	       (cond
		 ((equal (first param) "nodename")
		  (setf *nodename* (second param)))
		 ((equal (first param) "echo")
		  (push `(,(second param) ,(format nil "~{~a~^ ~}" (cdr (cdr param)))) *echoareas*)))))))
  (setf *echoareas* (reverse *echoareas*)))

(setf *dispatch-table*
      `(,(create-prefix-dispatcher "/" 'index)))

(defun echolist ()
  "/list.txt"
  (setf (content-type*) "text/plain")
  (format nil "~{~{~a:~a~}~%~}" *echoareas*))

(defun caesium ()
  "/x/caesium for caesium client"
  (setf (content-type*) "text/plain")
  (format nil "~{echo ~{~a ~a~}~%~}" *echoareas*))

(defun small-list ()
  "/x/small-echolist"
  (setf (content-type*) "text/plain")
  (format nil "~{~{~a ~*~}~%~}" *echoareas*))

(defun blacklist ()
  "/blacklist.txt"
  (setf (content-type*) "text/plain")
  (let (lines)
    (with-open-file (in "blacklist.txt")
      (loop for line = (read-line in nil)
	 while line do
	   (push line lines)))
    (format nil "~{~a~%~}" (reverse lines))))

(defun echoareas (echoarea)
  "/e/"
  (setf (content-type*) "text/plain")
  (format nil "~{~a~%~}" (get-echoarea echoarea)))

(defun message (msgid)
  "/m/"
  (setf (content-type*) "text/plain")
  (if (msg-filter msgid)
      (let ((msg (get-message msgid)))
	(if msg
	    (format nil "~{~a~%~}" msg)
	    (format nil "~a" "")))))

;;;
;;; Магия подсписков
;;;
(defun sublist (s e l)
  (let (ret)
    (if (or (not s) (not e))
	;; Если не задан один из параметров
	(progn
	  (when (and s (not e))
	    ;; Задан только начальный параметр
	    (setf e s)
	    (setf s 1))
	  (when (and e (not s))
	    ;; Задан только конечный параметр
	    (setf s (+ (- (length l) e) 1))))
	(progn
	  ;; Если начальный параметр меньше нуля
	  (if (< s 0)
	      (setf s (+ (length l) s 1)))
	  ;; Если сумма параметров больше длины списка
	  (if (> (- (+ s e) 2) (length l))
	      (setf e (+ (- (length l) s) 1)))))
    ;; Если начальный параметр указывает за пределы списка
    (if (or (< s 0) (> s (length l)))
	(setf s 1))
    ;; Если конечный параметр указывает за пределы списка
    (if (or (< e 0) (> e (length l)))
	(setf e (length l)))
    (loop for i from (- s 1) to (- (+ s e) 2) do
	 (push (nth i l) ret))
    (reverse ret)))

(defun u-echoareas (echoareas)
  "/u/e/"
  (setf (content-type*) "text/plain")
  (let (ret lines limits)
    (when (find #\: (car (last echoareas)) :test 'equal)
      (setf limits (split-sequence #\: (car (last echoareas))))
      (setf echoareas (reverse (cdr (reverse echoareas)))))
    (dolist (echoarea echoareas)
      (setf ret (append ret (list echoarea)))
      (setf lines (get-echoarea echoarea))
      (if limits
	  (let (s e)
	    (setf s (parse-integer (first limits) :junk-allowed t))
	    (setf e (parse-integer (second limits) :junk-allowed t))
	    (setf lines (sublist s e lines))))
      (setf ret (append ret lines)))
    (format nil "~{~a~%~}" ret)))

(defun bundle (msgids)
  "/u/m/"
  (setf (content-type*) "text/plain")
  (let (lines msg)
    (dolist (msgid msgids)
      (when (and
	     (msg-filter msgid)
	     (probe-file (make-pathname :name (concatenate 'string "msg/" msgid))))
	(setf msg (format nil "~{~a~%~}" (get-message msgid)))
	(push (concatenate 'string msgid ":"
			   (usb8-array-to-base64-string
			    (string-to-octets msg :external-format :utf8)))
	      lines)))
    (format nil "~{~a~%~}" (reverse lines))))

(defun echoareas-count (echoareas)
  (setf (content-type*) "text/plain")
  (let (counts)
    (dolist (echoarea echoareas)
      (push (concatenate 'string echoarea ":" (write-to-string (length (get-echoarea echoarea)))) counts))
    (format nil "~{~a~%~}" (reverse counts))))

(defun features ()
  (setf (content-type*) "text/plain")
  (format nil "~{~a~%~}" '("u/e" "list.txt" "blacklist.txt" "x/c" "x/file" "x/small-echolist" "x/caesium"))) 

(defun index()
  "Dispatcher for using beautyfull URLs."
  (let ((request (split-sequence #\/ (request-uri *request*))))
    (cond
      ((equal (second request) "list.txt")
       (echolist))
      ((and (equal (second request) "x") (equal (third request) "caesium"))
       (caesium))
      ((and (equal (second request) "x") (equal (third request) "small-echolist"))
       (small-list))
      ((equal (second request) "blacklist.txt")
       (blacklist))
      ((equal (second request) "e")
       (echoareas (third request)))
      ((equal (second request) "m")
       (message (third request)))
      ((and (equal (second request) "u") (equal (third request) "e"))
       (u-echoareas (cdddr request)))
      ((and (equal (second request) "u") (equal (third request) "m"))
       (bundle (cdddr request)))
      ((and (equal (second request) "x") (equal (third request) "c"))
       (echoareas-count (cdddr request)))
      ((and (equal (second request) "x") (equal (third request) "features"))
       (features)))))

(defvar *acceptor*
  (make-instance 'easy-acceptor :port 4242))

(load-config)
(start *acceptor*)
(format t "iicl started at address http://127.0.0.1:4242")
