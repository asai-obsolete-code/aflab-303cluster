#!/usr/local/bin/sbcl --script

(print "ITERATOR TEST")
(force-output)

(defun tarai (x y z)
  (if (<= x y)
      y
      (tarai (tarai (1- x) y z)
             (tarai (1- y) z x)
             (tarai (1- z) x y))))

(time (print (tarai 15 8 1)))



