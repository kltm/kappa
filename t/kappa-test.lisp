;;;; -*- mode: Lisp -*-
;;;;
;;;; Test web agent using hunchentoot and fiveam.
;;;;
;;;; NOTE: Can manually do that last bit with (lift:run-test), but it
;;;; seems to go automatically on a compile anyways.
;;;;

(defpackage :kappa-test
  (:use :cl
	:kappa)
  (:export
   ))
(in-package :kappa-test)

;;;
;;; Convenience.
;;;

(defmacro ccat (&rest strings) `(concatenate 'string ,@strings))

;;;
;;; Server controls.
;;;

(defparameter +server+ nil)
(defparameter +port+ 7357)
(defparameter +base+ (ccat "http://localhost:" (write-to-string +port+)))


;; Hunchentoot env.
(setf hunchentoot::*show-lisp-errors-p* t)

(defun stop-test-server ()
  (when +server+
    (handler-case
	(hunchentoot:stop +server+)
      (SIMPLE-ERROR (se) (declare (ignore se)) nil)
      (TYPE-ERROR (te) (declare (ignore te)) nil))
    (setf +server+ nil)))

(defun start-test-server ()
  (stop-test-server)
  (hunchentoot:define-easy-handler (main-test-page :uri "/test01.html") ()
    (setf (hunchentoot:content-type*) "text/html")
    (test01-html))
  (setf +server+ (make-instance 'hunchentoot:acceptor :port +port+))
  (hunchentoot:start +server+))

;;;
;;; Test pages.
;;;

(defun test01-html ()
  (cl-who:with-html-output-to-string (strm)
    (:html (:head
	    (:body
	     (:div :id "test01"
		   (:a :href "http://google.com" :title "foo" "link 01")
		   (:br)
	     (:div :id "test3")))))))

;;;
;;; Test definitions.
;;;
;;; To run these outside of the test suite, remember to use
;;; (start-test-server) and (stop-test-server).
;;;

(defun test-single-link ()
  "Test the links in our example document \"test01.html\"."
  (let ((a (make-instance 'agent)))
    (fetch a (ccat +base+ "/test01.html"))
    (lift:ensure-same 1 (length (links a)))))

;;;
;;; Run tests.
;;;
  
(lift:deftestsuite test-simple-page ()
  ()
  (:setup (start-test-server))
  (:teardown (stop-test-server)))

(lift:addtest (test-simple-page) (test-single-link))
