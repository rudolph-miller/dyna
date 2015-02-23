(in-package :cl-user)
(defpackage dyna-test.init
  (:use :cl)
  (:import-from :local-time
                :parse-timestring
                :*default-timezone*
                :+utc-zone+)
  (:export :make-clock))
(in-package :dyna-test.init)

(syntax:use-syntax :annot)

(setf *default-timezone* +utc-zone+)

(defstruct clock
  (year 1990)
  (month 1)
  (day 1)
  (hour 0)
  (minute 0)
  (sec 0))

(defmethod local-time:clock-now ((clock clock))
  (parse-timestring
   (format nil "~a-~a-~aT~a:~a:~a"
           (clock-year clock)
           (clock-month clock)
           (clock-day clock)
           (clock-hour clock)
           (clock-minute clock)
           (clock-sec clock))))

@export
(defmacro with-stub-now (clock &body body)
  `(let ((local-time:*clock* ,clock))
     ,@body))
