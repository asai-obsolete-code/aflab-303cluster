#! /usr/local/bin/sbcl --script

(let (plist)
  (handler-case
      (tagbody
        start
        (let ((read (read *standard-input* nil nil nil)))
          (incf (getf plist (first read) 0)))
        (go start))
    (print plist)))
