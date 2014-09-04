#! /usr/local/bin/sbcl --script

(load
 (merge-pathnames
             "quicklisp/setup"
             (user-homedir-pathname)))

(ql:quickload :iterate) (use-package :iterate)
(ql:quickload :optima)  (use-package :optima)
(ql:quickload :alexandria) (use-package :alexandria)



(let (plist solvers domains)
  (handler-case
      (iter (match (read *standard-input*)
              ((list* domain _ solver _)
               (incf (getf (getf plist solver) domain 0))
               (pushnew domain domains)
               (pushnew solver solvers))))
    (end-of-file (c)
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
                          coverage))))))

(terpri)
