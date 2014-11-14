#! /usr/local/bin/sbcl --script

(load
 (merge-pathnames
             "quicklisp/setup"
             (user-homedir-pathname)))

(eval (read-from-string
         "(push (merge-pathnames
         \"repos/lisp/\"
         (user-homedir-pathname))
        quicklisp-client:*local-project-directories*)"))

(with-open-file (*standard-output* "/dev/null"
                                   :direction :output
                                   :if-exists :overwrite)
  (ql:quickload :iterate) (use-package :iterate)
  (ql:quickload :optima)  (use-package :optima)
  (ql:quickload :osicat) ; (use-package :osicat)
  (ql:quickload :cl-ppcre) ; (use-package :osicat)
  (ql:quickload :alexandria) (use-package :alexandria)
  (ql:quickload :eazy-gnuplot) (use-package :eazy-gnuplot)
  (ql:quickload :associative-array) (use-package :associative-array))


;;; domains and solvers

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

(defvar *solver-blacklist* '(;; cea cea2 cea2ncp cea2ncy
                             ;; ff2ncp ff2ncy
                             ;; fd2ncp fd2ncy
                             ;; fffdncp fffdncy
                             ;; mv mv2
                             ;; solep
                             ))
(defvar *domain-blacklist* '(sokoban-sat11-strips transport-sat11-strips))

;;; main

;; (domname probname solver time mem 
;; elapsed preprocess usage
;; cost length macrousage metalength
;; forwardmacro cyclicmacro numeval numcomp)

(defun myprint (threshold)
  (let ((db (associative-array 3)))
    (handler-case
        (iter (match (read *standard-input*)
                ((list* domain prob solver _ _
                        (and data (list* elapsed _)))
                 (when (and (< elapsed (* 1000 threshold)) ;; msec->sec
                            (not (member solver *solver-blacklist*))
                            (not (member domain *domain-blacklist*)))
                   (setf (aaref db domain prob solver) data)))))
      (end-of-file (c)
        (let ((solvers (associative-array-dimension db 2)))
          (with-plots (*standard-output*)
            (gp-setup :xlabel (format nil "problem #")
                      :ylabel (format nil "preprocess / elapsed")
                      :output (format nil "preprocess-ratio.pdf")
                      :key '(:bottom :right :font "Times New Roman, 6")
                      :pointsize "0.4px"
                      :datafile '(:missing "nan"))
            (mapc (lambda (solver)
                    (plot (lambda ()
                            ;; compute preprocess ratio
                            (iter (for domain in (associative-array-dimension db 0))
                                  (iter (for prob in (sort (associative-array-dimension db 1)
                                                           #'string<))
                                        (format t "~&~a"
                                                (match (aaref db domain prob solver)
                                                  ((list* elapsed preprocess _)
                                                   (float (/ preprocess elapsed)))
                                                  (_ :nan))))))
                          :title (princ-to-string solver)
                          :with '(:points)))
                  solvers)))))))


(myprint (parse-integer (second sb-ext:*posix-argv*)))

(terpri)
