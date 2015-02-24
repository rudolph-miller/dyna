(in-package :cl-user)
(defpackage dyna.util
  (:use :cl)
  (:import-from :ironclad
                :digest-sequence
                :byte-array-to-hex-string
                :make-hmac
                :update-hmac
                :hmac-digest)
  (:import-from :flexi-streams
                :string-to-octets)
  (:import-from :cl-base64
                :usb8-array-to-base64-string)
  (:import-from :local-time
                :format-timestring
                :now)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :jsown
                :new-js
                :do-json-keys))
(in-package :dyna.util)

(syntax:use-syntax :annot)

@export
(defun to-octets (string)
  (etypecase string
    (string (string-to-octets string :external-format :utf-8))
    ((array (unsigned-byte 8)) string)))

@export
(defun to-base64 (string)
  (usb8-array-to-base64-string (to-octets string)))

@export
(defun timestamp ()
  (format-timestring nil (now)
                     :format '(:year (:month 2 #\0) (:day 2 #\0) "T" (:hour 2 #\0) (:min 2 #\0) (:sec 2 #\0) "Z")))

@export
(defun datestamp ()
  (format-timestring nil (now)
                     :format '(:year (:month 2 #\0) (:day 2 #\0))))

@export
(defun digest-sha256 (string)
  (byte-array-to-hex-string
   (digest-sequence :sha256 (to-octets string))))

@export
(defun hmac (key str)
  (let ((hmac (make-hmac (to-octets key) :sha256)))
    (update-hmac hmac (to-octets str))
    (hmac-digest hmac)))

@export
(defun sort-alist (lst)
  (flet  ((to-s (s)
            (etypecase s
              (symbol (symbol-name s))
              (string s))))
    (sort lst #'(lambda (a b)
                  (string< (to-s (car a)) (to-s (car b)))))))

@export
(defun parse-result-item (item)
  (let ((result))
    (do-json-keys (key val) item
      (push (cons key (parse-column (cadr val))) result))
    result))

(defun parse-column (column)
  (cond ((string= (car column) "N")
         (parse-integer (cdr column)))
        (t (cdr column))))

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

@export
(defun add-obj-to-list (lst)
  (cons :obj lst))

@export
(defun build-obj-list(lst)
  (mapcar #'(lambda (item)
              (add-obj-to-list item))
          lst))

@export
(defun build-desc-list (lst)
  (mapcar #'(lambda (pair)
              (cons (car pair)
                    (desc (cdr pair))))
          lst))

@export
(defun operation->opration-for-fetch (op)
  (format nil "~{~:(~a~)~}" (split-sequence #\- (symbol-name op))))
