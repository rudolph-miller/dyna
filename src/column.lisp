(in-package :cl-user)
(defpackage dyna.column
  (:use :cl
        :annot.class
        :dyna)
  (:import-from :closer-mop
                :validate-superclass
                :standard-direct-slot-definition
                :direct-slot-definition-class
                :class-direct-slots
                :slot-definition-name))
(in-package :dyna.column)

(syntax:use-syntax :annot)

@export
@export-accessors
(defclass <dyna-table-column> (standard-direct-slot-definition)
  ((key-type :initarg :key-type
             :initform nil)
   (attr-name :type (or string null)
              :initarg :attr-name
              :initform nil)
   (attr-type :initarg :attr-type
              :initform :nil)))

@export
(defgeneric key-type (column)
  (:method ((column <dyna-table-column>))
    (when (and (slot-boundp column 'key-type) (slot-value column 'key-type))
      (format nil "~:@(~a~)" (slot-value column 'key-type)))))

@export
(defgeneric attr-name (column)
  (:method ((column <dyna-table-column>))
    (if (and (slot-boundp column 'attr-name) (slot-value column 'attr-name))
        (slot-value column 'attr-name)
        (format nil "~(~a~)" (slot-definition-name column)))))

@export
(defgeneric attr-type (column)
  (:method ((column <dyna-table-column>))
    (when (and (slot-boundp column 'attr-type) (slot-value column 'attr-type))
      (format nil "~:@(~a~)" (slot-value column 'attr-type)))))
