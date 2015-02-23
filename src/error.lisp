(in-package :cl-user)
(defpackage dyna.error
  (:use :cl))
(in-package :dyna.error)

(syntax:use-syntax :annot)

@export
(define-condition <dyna-error> (simple-error) ())

@export
(define-condition <dyna-request-error> (<dyna-error>)
  ((meta :initarg :meta)
   (message :initarg :message))
  (:report
   (lambda (condition stream)
     (format stream
             "Error occured in request.~%Messssage: ~a~%Meta: ~a."
             (slot-value condition 'message)
             (slot-value condition 'meta)))))

@export
(define-condition <dyna-incomplete-argumet-error> (<dyna-error>)
  ((args :initarg :args))
  (:report
   (lambda (condition stream)
     (format stream
             "You must complete ~{:~a~^,~}."
             (slot-value condition 'args)))))
