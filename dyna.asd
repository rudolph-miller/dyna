#|
  This file is a part of dyna project.
  Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)
|#

#|
  Common Lisp library for AWS DynamoDB.

  Author: Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage dyna-asd
  (:use :cl :asdf))
(in-package :dyna-asd)

(defsystem dyna
  :version "0.1"
  :author "Rudolph-Miller"
  :license "MIT"
  :depends-on (:cl-syntax-annot
               :drakma
               :ironclad
               :flexi-streams
               :cl-base64
               :quri
               :local-time
               :jsown
               :split-sequence
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "dyna" :depends-on ("structure" "operation" "error"))
                 (:file "error")
                 (:file "util" :depends-on ("desc"))
                 (:file "desc" :pathname "describe")
                 (:file "request" :depends-on ("util"))
                 (:file "fetch" :depends-on ("request" "error"))
                 (:file "content" :depends-on ("structure" "desc"))
                 (:file "operation" :depends-on ("fetch" "content" "structure" "error"))
                 (:file "structure"))))
  :description "Common Lisp library for AWS DynamoDB."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op dyna-test))))
