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
                :*default-timezone*
                :+utc-zone+
                :format-timestring
                :now)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :jsown
                :new-js
                :do-json-keys
                :val
                :keyp)
  (:import-from :dyna.desc
                :desc))
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
  (let ((*default-timezone* +utc-zone+))
    (format-timestring nil (now)
                       :format '(:year (:month 2 #\0) (:day 2 #\0) "T" (:hour 2 #\0) (:min 2 #\0) (:sec 2 #\0) "Z"))))

@export
(defun datestamp ()
  (let ((*default-timezone* +utc-zone+))
    (format-timestring nil (now)
                       :format '(:year (:month 2 #\0) (:day 2 #\0)))))

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

@export
(defun class-inherit-p (target parent)
  (not (null
        (member parent
                (c2mop:class-direct-superclasses target)
                :test #'eq))))

@export
(defun safety-val (object key)
  (when (keyp object key) (val object key)))

@export
(defun gen-attr-table (list pre)
  (loop for key in list
        for i from 0
        collecting (cons (format nil "~a~a" pre i) key)))
