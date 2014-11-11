#! /usr/local/bin/sbcl --script

(load
 (merge-pathnames
             "quicklisp/setup"
             (user-homedir-pathname)))

(let ((*standard-output* *error-output*))
  (ql:quickload :iterate) (use-package :iterate)
  (ql:quickload :optima)  (use-package :optima)
  (ql:quickload :osicat) ; (use-package :osicat)
  (ql:quickload :cl-ppcre) ; (use-package :osicat)
  (ql:quickload :alexandria) (use-package :alexandria))

;; (barman p01 nf-fffd 3600 2000000 597 158815 233252 387 27.798)
;; (dom    prob solver maxt maxm   cost time   mem macrocost preprocess)

(defun cut (str max)
  (if (<= max (length str))
      (subseq str 0 max)
      str))

(defun max-problem-number (domain)
  (length
   (remove-if-not #'problem-p
                  (osicat:list-directory (princ-to-string domain)))))

(defun problem-p (path)
  (ppcre:scan "p[0-9]+\\.pddl" (file-namestring path)))

(defun rename-domain (domain)
  (case domain
    (barman 'barman-l)
    (cell-assembly-noneg-nocost 'assembly-mixed)
    (elev-lessfloors 'elev-9floors)
    (elev-20floor 'elev-20floors)       ; plural
    (elev-40floor 'elev-40floors)
    (gripper 'gripper-l)
    (openstacks-nocost 'openstacks-l)
    (rovers 'rovers-l)
    (satellite-typed2 'satellite-l)
    (barman 'barman-l)
    (woodworking-sat11-nocost 'woodworking-l)
    (pipesworld-notankage 'pipesworld-nt)
    (pipesworld-tankage 'pipesworld)
    (tpp 'tpp-l)
    (miconic 'miconic-l)
    (t domain)))

(defvar *nonipc-domains*
    '(barman
      cell-assembly-noneg-nocost
      elev-lessfloors
      elev-20floor       ; plural
      elev-40floor
      gripper
      openstacks-nocost
      rovers
      satellite-typed2
      woodworking-sat11-nocost
      tpp
      miconic))

(defun ipc-p (domain)
  (not (member domain *nonipc-domains*)))

(defun segfault-p (domain solver coverage)
  (and (member solver '(mv mv2))
       (member domain '(gripper miconic driverlog zenotravel mystery))
       (= coverage 0)))

(defun rename-solver (solver)
  (case solver
    (ff '\\ff)
    (ff2 '\\ffff)
    (ff2ncp '\\ffffncp)
    (ff2ncy '\\ffffncy)
    
    (fd '\\fd)
    (fd2 '\\fdfd)
    (fd2ncp '\\fdfdncp)
    (fd2ncy '\\fdfdncy)
    
    (cea '\\cea)
    (cea2 '\\cc)
    (cea2ncp '\\ccncp)
    (cea2ncy '\\ccncy)

    (fffd '\\fffd)
    (fffdncp '\\fffdncp)
    (fffdncy '\\fffdncy)
    
    (mv '\\mv)
    (mv2 '\\mvmv)
    (probe '\\pr)
    (probe2 '\\prpr)
    (solep '\\solep)
    (t solver)))

(defvar *solver-blacklist* '(cea cea2 cea2ncp cea2ncy
                             ff2ncp ff2ncy
                             fd2ncp fd2ncy
                             fffdncp fffdncy
                             mv mv2
                             solep))
(defvar *domain-blacklist* '(sokoban-sat11-strips transport-sat11-strips))

(defun hline (n)
  (format t "~&|---------------------|~{~a~}"
          (make-list n :initial-element "---------|")))

(defun header (string n)
  (format t "~&| ~20a|~{~a~}" string
          (make-list n :initial-element "         |")))

(defun myprint (threshold)
  (format t "~&#+ATTR_LATEX: :align |l|cc|cc|c|cc|cc|c|" threshold)
  (let (plist solvers domains)
    (handler-case
        (iter (match (read *standard-input*)
                     ((list* domain _ solver _ _ _ time _)
                      (when (and (< time (* 1000 threshold)) ;; msec->sec
                                 (not (member solver *solver-blacklist*))
                                 (not (member domain *domain-blacklist*)))
                        (incf (getf (getf plist solver) domain 0))
                        (pushnew domain domains)
                        (pushnew solver solvers)))))
      (end-of-file (c)
        (setf solvers (sort solvers #'string<))
        (setf domains (sort domains #'string<))
        (setf *print-case* :downcase)
        (hline (length solvers))
        (format t "~&| ~20a|~{ ~8a|~}"
                (format nil "~a[sec]" threshold)
                (mapcar #'rename-solver solvers))
        ;; (hline (length solvers))
        (header "EX-Large instances" (length solvers))
        (hline (length solvers))
        (labels ((print-domains (domains)
                   (iter (for domain in domains)
                         (format t "~&| ~20@<~a(~a)~>|"
                                 (princ-to-string
                                  (rename-domain domain))
                                 (max-problem-number domain))
                         (print-solvers domain)))
                 (print-solvers (domain)
                   (iter (with coverages =
                               (mapcar (lambda (solver)
                                         (getf (getf plist solver) domain 0))
                                       solvers))
                         (for solver in solvers)
                         (for coverage in coverages)
                         (cond
                           ((segfault-p domain solver coverage)
                            (format t " ~8@<-~>|"))
                           ((= coverage (reduce #'max coverages))
                            (format t " ~8@<*~a*~>|" coverage))
                           (t (format t " ~8@<~a~>|" coverage))))))
          (print-domains (remove-if #'ipc-p domains))
          (header "" (length solvers))
          (header "IPC instances" (length solvers))
          (hline (length solvers))
          (print-domains (remove-if-not #'ipc-p domains))
          (hline (length solvers)))))))

(myprint (parse-integer (second sb-ext:*posix-argv*)))

(terpri)
