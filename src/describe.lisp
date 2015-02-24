(in-package :cl-user)
(defpackage dyna.desc
  (:use :cl))
(in-package :dyna.desc)

(syntax:use-syntax :annot)

(defun string-desc (str)
  `(:obj ("S" . ,str)))

(defun string-set-desc (set)
  `(:obj ("SS" . ,set)))

(defun binary-desc (binary)
  `(:obj ("B" . ,binary)))

(defun binary-set-desc (set)
  `(:obj ("BS" . ,set)))

(defun bool-desc (bool)
  (let ((bool (if bool "true" "false")))
    `(:obj ("BOOL" . ,bool))))

(defun null-desc (null)
  (declare (ignore null))
  `(:obj "NULL"))

(defun number-desc (num)
  `(:obj ("N" . ,(write-to-string num))))

(defun number-set-desc (set)
  `(:obj ("NS" . ,(mapcar #'(lambda (item) (write-to-string item)) set))))

(defun list-desc (list)
  `(:obj ("L" . ,list)))

(defun map-desc (map)
  `(:obj ("M" . ,map)))

@export
(defun desc (object)
  (etypecase object
    (boolean (bool-desc object))
    (number (number-desc object))
    (string (string-desc object))
    (cons (if (every #'numberp object)
              (number-set-desc object)
              (string-set-desc object)))))
