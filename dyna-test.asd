#|
  This file is a part of dyna project.
  Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage dyna-test-asd
  (:use :cl :asdf))
(in-package :dyna-test-asd)

(defsystem dyna-test
  :author "Rudolph-Miller"
  :license "MIT"
  :depends-on (:dyna
               :prove
               :local-time)
  :components ((:module "t"
                :components
                ((:file "init")
                 (:test-file "dyna")
                 (:test-file "request")
                 (:test-file "fetch")
                 (:test-file "content")
                 (:test-file "table")
                 (:test-file "sxql")
                 (:test-file "table-operation"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
