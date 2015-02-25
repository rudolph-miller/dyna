(in-package :cl-user)
(defpackage dyna.table
  (:use :cl
        :dyna
        :dyna.operation
        :dyna.structure)
  (:import-from :alexandria
                :ensure-list)
  (:import-from :closer-mop
                :validate-superclass
                :standard-direct-slot-definition
                :direct-slot-definition-class
                :class-direct-slots
                :slot-definition-name))
(in-package :dyna.table)

(syntax:use-syntax :annot)

;; Column

(defclass <dyna-table-column> (standard-direct-slot-definition)
  ((key-type :type :keyword :initarg :key-type)
   (attr-name :type string :initarg :attr-name)))

(defgeneric key-type (column)
  (:method ((class <dyna-table-column>))
    (slot-value class 'key-type)))

(defgeneric attr-name (column)
  (:method ((class <dyna-table-column>))
    (slot-value class 'attr-name)))

;; Table

@export
(defclass <dyna-class> () ())

@export
(defclass <dyna-table-class> (standard-class)
  ((table-name :type string :initarg :table-name)
   (dyna :type dyna :initarg :dyna)))

(defmethod initialize-instance :around ((class <dyna-table-class>) &rest initargs)
  (initialize-action initargs)
  (call-next-method))

(defun initialize-action (initargs)
  (when (getf initargs :dyna)
    (setf (getf initargs :dyna)
          (eval (car (getf initargs :dyna))))))

(defmethod reinitialize-instance :around ((class <dyna-table-class>) &rest initargs)
  (initialize-action initargs)
  (call-next-method))

(defmethod validate-superclass ((class <dyna-table-class>) (super standard-class))
  t)

(defmethod direct-slot-definition-class ((class <dyna-table-class>) &key)
  '<dyna-table-column>)

(defgeneric table-name (class)
  (:method ((class <dyna-table-class>))
    (car (slot-value class 'table-name))))

(defgeneric table-hash-key (class)
  (:method (class)
    (find-key-type-key class :hash)))

(defgeneric table-range-key (class)
  (:method (class)
    (find-key-type-key class :range)))

(defgeneric find-key-type-key (class type)
  (:method ((class symbol) type)
    (find-key-type-key (find-class class) type))
  (:method ((class <dyna-table-class>) type)
    (find type (class-direct-slots class) :key #'key-type)))

(defgeneric table-dyna (class)
  (:method ((class <dyna-table-class>))
    (slot-value class 'dyna)))

(defun build-obj (class result)
  (loop with obj = (make-instance class)
        for slot in (class-direct-slots class)
        do (setf (slot-value obj (slot-definition-name slot))
                 (cdr (assoc (attr-name slot) result)))
        finally (return obj)))

;; Operation

(defgeneric find-dyna (class &rest values)
  (:method ((class symbol) &rest values)
    (apply #'find-dyna (find-class class) values))
  (:method ((class <dyna-table-class>) &rest values)
    (let ((hash-key (table-hash-key class))
          (range-key (table-range-key class)))
      (multiple-value-bind (result raw-result error)
          (get-item (table-dyna class) :table-name (table-name class)
                                       :key (append (list (cons (attr-name hash-key) (car values)))
                                                    (when range-key
                                                      (list (cons (attr-name range-key) (cadr values)))))
                                       :return-consumed-capacity "TOTAL")
        (values (build-obj class result) raw-result error)))))
