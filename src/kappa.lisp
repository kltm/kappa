;;;; -*- mode: Lisp -*-
;;;;
;;;; Web agent based loosely on WWW::Mechanize.
;;;;
;;;; Example for test suite: (setf l (make-link "http://mail.google.com/mail/?hl=de&hl=en&tab=wm&bar=&bib#foo"))
;;;;
;;;; URLs known to trip exception:
;;;;    http://flybase.bio.indiana.edu/reports/FBgn0086348.html
;;;;    http://www.ebi.ac.uk/cgi-bin/emblfetch?style=html&Submit=Go&id=CP000158
;;;;    http://biocyc.org/META/substring-search?type=NIL&object=PARATHION-DEGRADATION-PWY"
;;;;
;;;; For full arguments, see:
;;;; http://common-lisp.net/~loliveira/ediware/drakma/request.lisp
;;;;
;;;; NOTE: I decided closure-html is a pain in the neck--the parse
;;;; tree is now handled by hand.
;;;;

(defpackage :kappa
  (:use :cl
	:drakma)
  (:export
   ;; Classes.
   agent
   link
   ;; form # TODO
   ;; Agent slots.
   user-agent
   original-url ; before redirect
   current-url ; after redirect
   errors
   content
   code
   wait ; in seconds
   links
   ;; Link slots
   raw-url
   clean-url
   base-url
   ;; Agent methods.
   make-agent
   fill-with-content
   fetch
   purge
   ;; Link methods.
   make-link
   to-string
   authority
   query-string
   query-list
   ))
(in-package :kappa)


(defvar *version* "0.2.0" "This version of CL Kappa.")

(defparameter +user-agent+
  (format nil "CL Kappa ~a (over Drakma)" *version*)
  "User agent string.")
(defparameter +agent-timeout+ 300 ; five minute default
  "Force a waiting timeout after this many seconds.")
(defparameter +make-canonical+ t
  "Try and make found links correct relative to the current url.")

;;;
;;; NOTE: Change the parameters of the URI parse in puri--there are a
;;; lot of sites that refuse to follow the RFCs, but one has to work
;;; with them...
;;;

(setf puri:*strict-parse* nil) ; looks like it's in export
(defparameter puri::*illegal-characters*
  (puri::reserved-char-vector
   (remove #\| (remove #\# puri::*excluded-characters*))))
(defparameter puri::*strict-illegal-query-characters*
  (puri::reserved-char-vector
   (append '(#\?) (remove #\# puri::*excluded-characters*))))
(defparameter puri::*illegal-query-characters*
  (puri::reserved-char-vector
   puri::*excluded-characters* :except '(#\^ #\| #\#)))

;;;
;;; chunga variations.
;;;

;; TODO: push this down so it only shadows
(setf chunga:*accept-bogus-eols* t)

;;;
;;; flexi-streams variations.
;;;

(setf flexi-streams:*substitution-char* #\?)

;;;
;;; Form: complicate our lives.
;;;

;; Subclass of nothing.
(defclass form ()
  ((action
    :accessor action
    :initarg :action
    :initform (error ":action is a required slot"))
   (id
    :accessor id
    :initform nil)
   (verb
    :accessor verb
    :initform :GET
    :documentation "GET (:get) unless otherwise stated.")
   (name
    :accessor name
    :initform nil)
   (inputs
    :accessor inputs
    :initform '())))

;; Constructor.
(defun make-forms-from-doc (html-doc)
  "Returns a list of forms from the input html document."
  (let ((pre-forms (get-form-properties-from-doc html-doc)))
    (mapcar (lambda (x) (make-form-from-properties x)) pre-forms)))

;; Helper.
(defun make-form-from-properties (form-props)
  "Given a form propery list, make a form."
  (let ((new-form (make-instance 'form :action (getf form-props :action))))
    (setf (slot-value new-form 'id) (getf form-props :id))
    (setf (slot-value new-form 'verb) (getf form-props :method))
    (setf (slot-value new-form 'name) (getf form-props :name))
    ;; Scan through by type and add "generic" if not otherwise defined.
    (dolist (input (getf form-props :inputs))
      (let ((input-type (getf input :type)))
	(when input-type
	  (setf input-type (string-upcase input-type)))
	;;(format t "~A~%" input-type)
	(cond
	  ((or (string= input-type "CHECKBOX")
	       (string= input-type "RADIO")
	       (string= input-type "FILE")
	       (string= input-type "HIDDEN")
	       (string= input-type "SELECT") ; artificial
	       (string= input-type "TEXTAREA")) ; artificial
	   (push input (inputs new-form)))
	  (t ; filter out ignorables, the rest should be text variants
	   (when (not (or (string= input-type "BUTTON")
			  (string= input-type "SUBMIT")
			  (string= input-type "IMAGE")
			  (string= input-type "RESET")))
	     ;; Add the fact that this is an unknown "generic".
	     (push (append input (list :TYPE "generic")) (inputs new-form)))))))
    new-form))

;; Helper.
(defun get-options-from-subtree (subtree)
  "..."
  (let ((whittled-list '()))
    (labels ((option-search (tree)
	       (cond
		 ((atom tree) nil)
		 ((eq :OPTION (car tree))
		  (push (alexandria:flatten (cadr tree)) whittled-list))
		 (t (mapcar #'option-search tree)))))
      (option-search subtree))
    whittled-list))

;; Helper.
(defun get-things-from-subtree (subtree)
  "..."
  (let ((whittled-list '()))
    (labels ((input-search (tree)
	       (cond
		 ((atom tree) nil)
		 ((eq :INPUT (car tree))
		  (push (alexandria:flatten (cadr tree)) whittled-list))
		 ((eq :TEXTAREA (car tree)) ; add textarea type since empty
		  (push (append '(:type "textarea")
				(alexandria:flatten (cadr tree)))
			whittled-list))
		 ((eq :SELECT (car tree)) ; collect options
		  (dolist (sub-selects (get-options-from-subtree tree))
		    (push (append sub-selects
				  '(:type "select")
				  (alexandria:flatten (cadr tree)))
			  whittled-list)))
		 (t (mapcar #'input-search tree)))))
      (input-search subtree))
    whittled-list))

;; Helper.
(defun get-form-properties-from-doc (html-doc)
  "Get for properties from an html document."
  (let ((whittled-list '()))
    (labels ((form-search (tree)
	       (cond
		 ((atom tree) nil)
		 ((eq :FORM (car tree))
		  (push (append (alexandria:flatten (cadr tree))
				(list :inputs
				      (get-things-from-subtree (cdr tree))))
			whittled-list))
		 (t (mapcar #'form-search tree)))))
      (form-search (chtml:parse html-doc (chtml:make-lhtml-builder))))
    whittled-list))

(defgeneric to-drakma-parameters (form)
  (:documentation "Turn a form into a Drakma form parameters list."))

(defmethod to-drakma-parameters ((form form))
  "Turn a form into a Drakma form parameters list."
  (let ((parameters '()))
    ;; Simple--everything should have some kind of type
    (dolist (input (inputs form))
      (let ((input-type (string-upcase (getf input :type))))
	(cond
	  ;; The easy cases.
	  ((or (string= input-type "TEXT")
	       (string= input-type "TEXTAREA") ; artificial
	       (string= input-type "HIDDEN")
	       (string= input-type "GENERIC")) ; artificial
	   (push (cons (getf input :name) (getf input :value)) parameters))
	  ;; NOTE: Hrm...files...using Drakma's trivial version.
	  ;; See: http://weitz.de/drakma/#parameters
	  ((string= input-type "FILE")
	   (when (getf input :path)
	     (push (cons (getf input :name)
			 (cl-fad:pathname-as-file (getf input :path)))
		   parameters)))
	  ;; Checkboxes and not too bad--one condition: they're checked.
	  ((or (string= input-type "CHECKBOX")
	       (string= input-type "RADIO"))
	   (when (string-equal (getf input :checked) "CHECKED")
	     (push (cons (getf input :name) (getf input :value)) parameters)))

	  ((string= input-type "SELECT")
	   (when (string-equal (getf input :selected) "SELECTED")
	     (push (cons (getf input :name) (getf input :value)) parameters)))
	  (t
	   (error (concatenate 'string "slipped through with: " input-type))))))
    parameters))

(defun plist-subset-p (plist checklist &key (test #'eq))
  "Checks that the properties are "
  ;;(format t "current: ~A ~A ~A~%" plist checklist test)
  (cond
    ((null checklist) t) ; trivially true
    ((and (getf plist (car checklist))
	  (apply test (list (getf plist (car checklist)) (cadr checklist))))
     (plist-subset-p plist (cddr checklist) :test test))
    (t nil)))

(defgeneric get-inputs (form &rest filter-plist)
  (:documentation "List all possible form inputs. Optionally by filter."))

;; (get-inputs f2) ; all inputs
;; (get-inputs f2 :TYPE "radio")
(defmethod get-inputs ((form form) &rest filter-plist)
  "..."
  (remove-if-not
   (lambda (r)
     (if (plist-subset-p r filter-plist :test #'string-equal) t nil))
   (inputs form)))

(defgeneric change-input-property (form property new-value &rest filters-plist)
  (:documentation "Change an input property to a new value."))

;; In form f1, change the :value property ot "yes" from the filters...
;; (change-input f1 :value "yes" :type "hidden" :name "force")
(defmethod change-input-property ((form form) property new-value
				  &rest filters-plist)
  "All matching changes will be made."
  (let ((inputs-to-change (apply 'get-inputs (cons form filters-plist))))
    (mapcar
     (lambda (input) (setf (getf input property) new-value))
     inputs-to-change)))

(defgeneric add-input-property (form new-prop new-val &rest plist-def)
  (:documentation "Add a new input proprty to the form."))

;; (add-input-property f2 :foo "bar" :class "radio")
(defmethod add-input-property ((form form) new-prop new-value &rest plist-def)
  "All matching changes will be made."
  ;; Capture the inputs I want to add to.
  (let ((inputs-to-add-to (apply 'get-inputs (cons form plist-def))))
    ;; Remove them to make room for the dopplegangers.
    (apply 'remove-input (cons form plist-def))
    ;; Add the dopplegangers to replace the originals.
    (mapcar
     (lambda (r)
       (apply #'add-input (append (list form new-prop new-value) r)))
     inputs-to-add-to)))

(defgeneric remove-input-property (form del-prop &rest plist-def)
  (:documentation "Remove an input from the form."))

;; (remove-input-property f2 :class :type "radio")
(defmethod remove-input-property ((form form) del-prop &rest plist-def)
  "All matching changes will be made."
  ;; Capture the inputs I want to remove from.
  (let ((inputs-to-remove-from (apply 'get-inputs (cons form plist-def))))
    ;; Remove them to make room for the dopplegangers.
    (apply 'remove-input (cons form plist-def))
    ;; Add the dopplegangers to replace the originals.
    (mapcar
     (lambda (r)
       (remf r del-prop)
       (apply #'add-input (cons form r)))
     inputs-to-remove-from)))

(defgeneric add-input (form &rest plist-def)
  (:documentation "Add a new input to the form."))

;; (add-input f2 :type "nothing" :value "nothing")
(defmethod add-input ((form form) &rest plist-def)
  "..."
  (setf (inputs form) (cons plist-def (inputs form))))

(defgeneric remove-input (form &rest plist-def)
  (:documentation "Remove an input from the form."))

;; (remove-input f1 :type "nothing")
(defmethod remove-input ((form form) &rest plist-def)
  "All inputs that match the plist-def will be removed."
  (setf (inputs form)
	(remove-if
	 (lambda (r)
	   (plist-subset-p r plist-def :test #'string-equal))
	   (inputs form))))

;;;
;;; Link: convenience layer over puri.
;;;

;;
(defclass link (puri:uri)
  ((raw-url
    :accessor raw-url
    :initform nil)
   (clean-url
    :accessor clean-url
    :initform nil)
   (base-url
    :accessor base-url
    :initform nil)))

;; Constructor.
(defun make-link (url-str)
  "Constructor for links."
  (let ((new-link (puri:parse-uri url-str :class 'link)))
    (setf (slot-value new-link 'raw-url) url-str)
    (let* ((scheme (string-downcase (symbol-name (puri:uri-scheme new-link))))
	   (server (puri:uri-authority new-link))
	   (path (puri:uri-path new-link))
	   (new-base (concatenate 'string scheme "://" server path))
	   (new-query (format nil "~{~A~^&~}"
			      (mapcar (lambda (x)
					(concatenate 'string
						     (car x) "=" (cdr x)))
				      (query-list new-link)))))
      (setf (slot-value new-link 'base-url) new-base)
      (setf (slot-value new-link 'clean-url)
	    (if (and new-query (not (string= new-query "")))
		(concatenate 'string new-base "?" new-query)
	    new-base))
      new-link)))

(defgeneric to-string (link)
  (:documentation "Turn a link into a string."))

(defmethod to-string ((link link))
  "..."
  (puri:render-uri link nil))

(defgeneric authority (link)
  (:documentation "Return the URI authority (server plus port (when not 80))."))

(defmethod authority ((link link))
  "..."
  (puri:uri-authority link))

(defgeneric query-string (link)
  (:documentation "..."))

(defmethod query-string ((link link))
  "..."
  (puri:uri-query link))

(defgeneric query-list (link)
  (:documentation "Return the query args as an ordered alist."))

(defmethod query-list ((link link))
  "..."
  (sort (mapcar (lambda (l)
		  (let ((split (cl-ppcre:split "=" l)))
		    (if (= 2 (length split))
			(cons (car split) (cadr split))
			(cons (car split) ""))))
		(cl-ppcre:split "&" (query-string link)))
	#'compare-alist-items))

;;;
;;; Agent.
;;;

(defclass agent ()
  ((user-agent
    :accessor user-agent
    :initform +user-agent+
    :initarg :user-agent)
   (original-url
    :accessor original-url
    :initform nil)
   (current-url
    :accessor current-url
    :initform nil)
   (errors
    :accessor errors
    :initform '())
   (content
    :accessor content
    :initform nil)
   (code
    :accessor code
    :initform nil)
   (wait
    :accessor wait
    :initform nil)
   (links
    :accessor links
    :initform '())
   (forms
    :accessor forms
    :initform '())))

;; Constructor.
(defun make-agent ()
  "Constructor for agents."
  (make-instance 'agent))

(defgeneric fill-with-content (agent content)
  (:documentation "Fill what we can of the agent from a string. Could be useful in non-web cases."))

(defmethod fill-with-content ((agent agent) content)
  "Things to do if we get a proper text body."
  (setf (content agent) content)
  (setf (forms agent) (make-forms-from-doc content))
  (let ((raw-links (extract-links content)))
    ;; Optionally make the links full (canonical) before processing.
    (when +make-canonical+
      (setf raw-links
	    (mapcar (lambda (x)
		      ;;(format t "c: [~A] [~A]~%" x (current-url agent))
		      (make-canonical x :relative-to (current-url agent)))
		    raw-links)))
    ;; ;; Optionally remove the fragment before processing.
    ;; (when +clean-links+
    ;;   ;; Remove jumps.
    ;;   (setf raw-links (mapcar (lambda (x)
    ;; 				(car (cl-ppcre:split "\#" x)))
    ;; 			      raw-links)))
    (setf (links agent) (mapcar (lambda (x) (make-link x)) raw-links))))

(defgeneric fetch (agent link)
  (:documentation "Run a tanuki agent against a specified url. This is
  mostly to get the document body and check for errors. For a more
  broad use (e.g. links), see run."))

;; Helper for catching errors while trying to fetch something.
(defmacro with-fetch-attempt (agent timeout &body body)
  "Fill out the body of the agent, as well as error catching."
  `(labels ((glitch (err-str)
	      (setf (errors ,agent) (cons err-str (errors ,agent)))
	      nil))
     ;; Try and use drakma in a minimal way and catch resolution errors
     ;; (with the above function).
     (handler-case
	 (progn
	   ;; Trivially set to 0 before we try.
	   (setf (wait ,agent) 0)
	   ;; Start timer.
	   (let ((start-time (get-universal-time)))
	     ;; Make sure we can bail after a while.
	     (trivial-timeout:with-timeout (,timeout)
	     (multiple-value-bind
		   (body response-code headers puri stream must-close-p reason)
		 ,@body
	       (declare (ignore stream must-close-p reason))
	       (cond
		 ;; TODO: Doesn't deal with images (etc.) real well yet.
		 ((search "png" (cdr (assoc :CONTENT-TYPE headers))) "")
		 ((search "gif" (cdr (assoc :CONTENT-TYPE headers))) "")
		 ((search "pdf" (cdr (assoc :CONTENT-TYPE headers))) "")
		 ((search "word" (cdr (assoc :CONTENT-TYPE headers))) "")
		 ;; TODO: Or, for that matter, tarballs.
		 ((search "tgz" (cdr (assoc :CONTENT-TYPE headers))) "")
		 ((search "bz2" (cdr (assoc :CONTENT-TYPE headers))) "")
		 ((search "zip" (cdr (assoc :CONTENT-TYPE headers))) "")
		 ((search "gzip" (cdr (assoc :CONTENT-TYPE headers))) "")
		 ((search "gz" (cdr (assoc :CONTENT-TYPE headers))) "")
		 (t (progn
		      ;; There may be a redirect, so set current URL
		      ;; to what comes back from Drakma.
		      (setf (current-url ,agent)
			    (puri:render-uri puri nil))
		      ;; Now fill the agent woth body contents.
		      (fill-with-content ,agent body))))
	       (setf (code ,agent) response-code)
	       ;; Return if we made it without an error.
	       t))
	     (setf (wait ,agent) (- (get-universal-time) start-time))))
       (DRAKMA:PARAMETER-ERROR
	   (pe) (declare (ignore pe))
	   (glitch "Possibly bad URL schema?"))
       (END-OF-FILE
	   (eof) (declare (ignore eof))
	   (glitch "EOF--reached end of file?"))
       (PURI:URI-PARSE-ERROR
	   (upe) (declare (ignore upe))
	   (glitch "uri parse error"))
       (RUNES-ENCODING:ENCODING-ERROR
	   (ree) (declare (ignore ree))
	   (glitch "appears to be some type of encoding issue"))
       (CHUNGA:SYNTAX-ERROR
	   (se) (declare (ignore se))
	   (glitch "chunga: syntax error"))
       (USOCKET:NS-HOST-NOT-FOUND-ERROR
	   (nhnfe) (declare (ignore nhnfe))
	   (glitch "internet not connected(?)"))
       (USOCKET:TIMEOUT-ERROR
	   (ute) (declare (ignore ute))
	   (glitch "server(?) timeout"))
       (USOCKET:HOST-UNREACHABLE-ERROR
	   (hue) (declare (ignore hue))
	   (glitch "host/server unreachable"))
       (USOCKET:CONNECTION-REFUSED-ERROR
	   (cre) (declare (ignore cre))
	   (glitch "host/server connection refused"))
       (trivial-timeout:timeout-error
	   (toe) (declare (ignore toe))
	   (glitch "client timeout"))
       #+(or sbcl)
       (SB-KERNEL::HEAP-EXHAUSTED-ERROR
	   (hee) (declare (ignore hee))
	   (glitch "heap exhausted--page too big?"))
       #+(or sbcl)
       (SB-BSD-SOCKETS::NO-ADDRESS-ERROR
	   (nae) (declare (ignore nae))
	   (glitch "no (such?) address"))
       #+(or sbcl)
       (SB-BSD-SOCKETS:NAME-SERVICE-ERROR
	   (nse) (declare (ignore nse))
	   (glitch "name service error: could not resolve DNS?"))
       #+(or sbcl)
       (SB-KERNEL:CASE-FAILURE
	   (cf) (declare (ignore cf))
	   (glitch "case failure--what is this?")))))

(defmethod fetch ((agent agent) link)
  "Fetch a URL with a string for the link argument."
  (purge agent)
  (setf (original-url agent) link)
  (with-fetch-attempt agent +agent-timeout+
    (http-request link :redirect 100 :user-agent (user-agent agent))))

(defgeneric purge (agent)
  (:documentation "Try and get an agent back to a \"pristine\" state"))

(defmethod purge ((agent agent))
  "Purge emphemeral materials from the agent."
  (setf (original-url agent) nil)
  (setf (current-url agent) nil)
  (setf (errors agent) '())
  (setf (content agent) nil)
  (setf (code agent) nil)
  (setf (wait agent) nil)
  (setf (links agent) '())
  (setf (forms agent) '())
  t)

(defgeneric list-forms (agent &optional by-type)
  (:documentation "List the forms currently held by the agent."))

(defmethod list-forms ((agent agent) &optional (by-type 'name))
  (mapcar (lambda (f) (apply by-type (list f))) (forms agent)))

(defgeneric get-form (agent identifier &optional by-type)
  (:documentation "Return a form currently held by the agent."))

(defmethod get-form ((agent agent) identifier &optional (by-type 'name))
  "Returns the first identified form and the number of matching forms."
  (let ((matching-forms
	 (remove-if-not
	  (lambda (f)
	    (if (string= (apply by-type (list f)) identifier) t nil))
	  (forms agent))))
    (values (car matching-forms) (length matching-forms))))

(defgeneric submit (agent form)
  (:documentation "Submit a form to the server and start anew."))

(defmethod submit ((agent agent) (form form))
  "Much the same as fetch, but with a form as the action base."
  ;; Get the things I want out of the agent before purging.
  (let ((old-url (current-url agent))
	(action (action form))
	(verb (verb form))
	(parameters (to-drakma-parameters form))) ; to be used as drakma's
    (purge agent) ; Clean out old stuff.
    ;; Straighten links.
    (setf (current-url agent) (make-canonical action :relative-to old-url))
    ;; Run.
    (with-fetch-attempt agent +agent-timeout+
      (http-request
       (current-url agent)
       ;; Verbs need to be either :POST or :GET
       :method (if (or (eq verb :POST) (string-equal verb "POST")) :POST :GET)
       :redirect 100
       :parameters parameters
       :user-agent (user-agent agent)))))

;;;
;;; Unexported helper functions.
;;;

(defun compare-alist-items (first second)
  (if (string= (car first) (car second))
      (string< (cdr first) (cdr second))
      (string< (car first) (car second))))

(defun make-rational (url-str)
  "Changes a url string into a proper url one way or another. This is
a cleaner for possibly odd, but mostly correct, urls."
  (puri:render-uri (puri:parse-uri url-str) nil))

(defun make-canonical (url-str &key (relative-to))
  "This merges the incoming url string with a relative url string. It
tries to make sense of partial urls, such as in links. Should lead to
some interesting bugs..."
  (make-rational
   (puri:merge-uris (puri:parse-uri url-str) (puri:parse-uri relative-to))))

;; Move along the list and pull out likely triplets.
;; (defun scan-out-links (xlist)
;;   "Worthless without a flattened tree from chtml:parse."
;;   (let ((hrefs '())
;;         (len (length xlist)))
;;     (when (>= len 3)
;;       (dotimes (i (- len 3))
;; 	(let ((1st  (elt xlist i))
;; 	      (2nd (elt xlist (+ 1 i)))
;; 	      (3rd (elt xlist (+ 2 i))))
;;           (if (and (eq :A 1st) (eq :HREF 2nd)
;;                    (stringp 3rd))
;;               (push 3rd hrefs)))))
;;     hrefs))

;; (defun extract-links (doc)
;;   "Hackily extract links strings from an HTML document."
;;   (when doc
;;     (let ((flattened-tree (alexandria:flatten
;;                            (chtml:parse doc (chtml:make-lhtml-builder)))))
;;       (scan-out-links flattened-tree))))

;; URLs in hrefs can have whitespace.
;; https://www.w3.org/TR/2014/REC-html5-20141028/infrastructure.html#valid-url-potentially-surrounded-by-spaces
(defun trim (str)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) str))

(defun extract-links (doc)
  "Will not include nulls, jump-onlys, and FTP."
  (let ((try-list (get-link-like-things doc)))
    (mapcar #'trim (sane-urls-only try-list))))

(defun get-link-like-things (doc)
  (let ((whittled-list '()))
    (labels ((find-a-href (ast) (getf (alexandria:flatten ast) :HREF))
	     (href-search (tree)
			  (cond
			   ((atom tree) nil)
			   ((eq :A (car tree))
			    (push (find-a-href (cadr tree)) whittled-list))
			   (t (mapcar #'href-search tree)))))
      (href-search (chtml:parse doc (chtml:make-lhtml-builder))))
    whittled-list))

(defun sane-urls-only (list)
  "Remove urls that are likely bogus."
  (remove-if #'(lambda (x)
		 (handler-case
		  (progn
		    (puri:parse-uri x) ; make sure edible by puri
		    (or
		     (not (> (length x) 0)) ; bigger than 0
		     (cl-ppcre::scan "^#" x) ; not a JS hash trick
		     (cl-ppcre::scan "^ftp\:\/\/" x) ; not ftp
		     (cl-ppcre::scan "^mailto\:" x) ; not "mailto"
		     (cl-ppcre::scan "^feed\:\/\/" x) ; not a "feed" link
		     (cl-ppcre::scan "^javascript\:" x) ; not a "js" link
		     (cl-ppcre::scan "^javascript\:\/\/" x))) ; not a "js" link
		  (PURI:URI-PARSE-ERROR (upe) (declare (ignore upe)) t)))
	     list))

;;;
;;; These should be off to tests or something.
;;;

;; (defun %reset ();;
;;   (fetch a "http://localhost/cgi-bin/amigo_1_8/slimmer")
;;   (setf f1 (car (forms a)))
;;   (setf f2 (cadr (forms a))))

;; Slimmer run. (spin out)
;; (time (%slim-run))
(defun %slim-run (&optional (location "http://localhost/cgi-bin/amigo/slimmer"))
  (let ((+agent-timeout+ 900)
	(a (make-agent))
	(f1 nil))
    (fetch a location)
    (setf f1 (car (forms a)))
    ;;(setf f2 (cadr (forms a)))
    (add-input-property
     f1 :path "/tmp/gene_association.mgi" :name "gp_list")
    (add-input-property
     f1 :selected "selected" :name "gp_file_type" :value "ga")
    (remove-input-property
     f1 :selected :name "subset" :selected "selected")
    (add-input-property
     f1 :selected "selected" :name "subset" :value "goslim_generic")
    (add-input-property
     f1 :selected "selected" :name "speciesdb" :value "mgi")
    (submit a f1)
    a))
