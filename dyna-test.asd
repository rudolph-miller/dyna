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
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "dyna"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
