(in-package :cl-user)
(defpackage dyna-test.init
  (:use :cl
        :dyna)
  (:import-from :local-time
                :parse-timestring
                :*default-timezone*
                :+utc-zone+)
  (:export :make-clock))
(in-package :dyna-test.init)

(syntax:use-syntax :annot)

(setf *default-timezone* +utc-zone+)

@export
(defvar *dyna* (make-dyna :credentials (cons "DYNA_TEST_ACCESS_KEY" "DYNA_TEST_SECRET_KEY")
                          :region "local"))

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

@export
(defun build-json (&rest lst)
  (format nil "{~{~a~^,~}}" lst))

@export
(defun init-dynamo-local ()
  (when (find "Thread" (list-tables *dyna*) :test #'equal)
    (delete-table *dyna* :table-name "Thread"))
  (create-table *dyna* :table-name "Thread"
                       :key-schema '((("AttributeName" . "ForumName") ("KeyType" . "HASH"))
                                     (("AttributeName" . "Subject") ("KeyType" . "RANGE")))
                       :attribute-definitions '((("AttributeName" . "ForumName") ("AttributeType" . "S"))
                                                (("AttributeName" . "Subject") ("AttributeType" . "S"))
                                                (("AttributeName" . "LastPostDateTime") ("AttributeType" . "S")))
                       :local-secondary-indexes '((("IndexName" . "LastPostDateTime")
                                                   ("KeySchema" . ((("AttributeName" . "ForumName")
                                                                    ("KeyType" . "HASH"))
                                                                   (("AttributeName" . "LastPostDateTime")
                                                                    ("KeyType" . "RANGE"))))
                                                   ("Projection" . (("ProjectionType" . "ALL")))))
                       :provisioned-throughput '(("ReadCapacityUnits" . 5)
                                                 ("WriteCapacityUnits" . 5))))
