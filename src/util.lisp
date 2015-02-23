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
                :now))
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
(defun alist-sort (lst)
  (flet  ((to-s (s)
            (etypecase s
              (symbol (symbol-name s))
              (string s))))
    (sort lst #'(lambda (a b)
                  (string< (to-s (car a)) (to-s (car b)))))))
