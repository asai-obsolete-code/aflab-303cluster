#! /usr/local/bin/sbcl --script

(load
 (merge-pathnames
             "quicklisp/setup"
             (user-homedir-pathname)))

(let ((*standard-output* *error-output*))
  (ql:quickload :iterate) (use-package :iterate)
  (ql:quickload :optima)  (use-package :optima)
  (ql:quickload :alexandria) (use-package :alexandria))

;; (barman p01 nf-fffd 3600 2000000 597 158815 233252 387 27.798)
;; (dom    prob solver maxt maxm   cost time   mem macrocost preprocess)


(defun myprint (threshold)
  (print threshold)
  (let (plist solvers domains)
    (handler-case
        (iter (match (read *standard-input*)
                     ((list* domain _ solver _ _ _ time _)
                      (when (< time (* 1000 threshold)) ;; msec->sec
                        (incf (getf (getf plist solver) domain 0))
                        (pushnew domain domains)
                        (pushnew solver solvers)))))
      (end-of-file (c)
        (setf solvers (sort solvers #'string<))
        (setf domains (sort domains #'string<))
        (format t "~&| ~30a|~{ ~12a|~}" 'domains solvers)
        (iter (for domain in domains)
              (format t "~&| ~30a|" domain)
              (iter (with coverages =
                          (mapcar (lambda (solver)
                                    (getf (getf plist solver) domain 0))
                                  solvers))
                    (for coverage in coverages)
                    (format t " ~12@<~:[~a~;*~a*~]~>|"
                            (= coverage (reduce #'max coverages))
                            coverage)))))))

(myprint (parse-integer (second sb-ext:*posix-argv*)))

(terpri)
