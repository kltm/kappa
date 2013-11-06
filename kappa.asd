;;;; -*- mode: Lisp -*-
;;;;
;;;; Quicklisp is already loaded through the quicklisp/setup.lisp file
;;;; in .sbclrc.
;;;;
;;;; To load, with this asdf in path, (require :kappa).
;;;;


(defpackage :kappa-asd
  (:use :cl :asdf))
(in-package :kappa-asd)

;;
(defsystem kappa
  :name "CL Kappa."
  :version "0.8.0"
  :author "Seth Carbon <lisp@genkisugi.net>"
  :maintainer "Seth Carbon <lisp@genkisugi.net>"
  :licence "Modified BSD"
  :description "General purpose web agent inspired by WWW::Mechanize."
  :long-description "General purpose web agent inspired by WWW::Mechanize."
  :components ((:static-file "kappa.asd")
               (:module :src
                        :serial t
                        :components ((:file "kappa"))))
  :depends-on (:cl-ppcre
	       :closure-html
	       :puri
	       :drakma
	       :trivial-timeout
	       :alexandria
	       :cl-fad
	       :fiveam)

  ;; TODO: Auto doc info...
)

;; Add kappa to our features.
(defmethod asdf:perform :after ((op asdf:load-op)
				(system (eql (asdf:find-system :kappa))))
  (pushnew :kappa cl:*features*))

;; (defmethod asdf:perform ((o test-op) (c (eql (find-system :kappa))))
;;   (asdf:operate 'asdf:test-op :kappa-test))

;; Describe the testing system.
(defsystem :kappa-test
  :components ((:module :t
                        :serial t
                        :components ((:file "kappa-test"))))
  :depends-on (:kappa
	       :lift
	       :cl-who
	       :hunchentoot))

;; ;; TODO: Not sure exactly what this magical incanation does, no doubt
;; ;; running tests on load of kappa.
;; (defmethod asdf:perform ((o test-op) (c (eql (find-system :kappa-test))))
;;   (asdf:operate 'asdf:load-op :kappa-test)
;;   (funcall (intern (symbol-name :run-tests)
;;                    (find-package :kappa-test))))
