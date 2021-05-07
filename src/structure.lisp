(in-package :cl-user)
(defpackage dyna.structure
  (:use :cl
        :annot.class))
(in-package :dyna.structure)

(syntax:use-syntax :annot)

@export
@export-accessors
(defstruct dyna
  (credentials)
  (token nil)
  (region "us-east-1"))
