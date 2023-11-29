
(in-package :cl-metal.test)

(deftest metalize-form
  (ok (string=
       "(a == 1);"
       (with-metalize ()
		      (= A 1)))))

