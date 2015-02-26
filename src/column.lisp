(in-package :cl-user)
(defpackage dyna.column
  (:use :cl
        :dyna)
  (:import-from :closer-mop
                :validate-superclass
                :standard-direct-slot-definition
                :direct-slot-definition-class
                :class-direct-slots
                :slot-definition-name))
(in-package :dyna.table)

(syntax:use-syntax :annot)

@export
(defclass <dyna-table-column> (standard-direct-slot-definition)
  ((key-type :type :keyword :initarg :key-type)
   (attr-name :type string :initarg :attr-name)))

@export
(defgeneric key-type (column)
  (:method ((class <dyna-table-column>))
    (slot-value class 'key-type)))

@export
(defgeneric attr-name (column)
  (:method ((class <dyna-table-column>))
    (slot-value class 'attr-name)))
