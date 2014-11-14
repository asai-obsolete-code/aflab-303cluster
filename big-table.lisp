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
(let ((*standard-output* *error-output*))
  (ql:quickload :iterate) (use-package :iterate)
  (ql:quickload :optima)  (use-package :optima)
  (ql:quickload :osicat) ; (use-package :osicat)
  (ql:quickload :cl-ppcre) ; (use-package :osicat)
  (ql:quickload :alexandria) (use-package :alexandria)
  (ql:quickload :associative-array) (use-package :associative-array))

;;; functions

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

;;; tex

(defun remove-keywords (args)
  (let ((noopt (remove '&optional args)))
    (iter nil
          (for sublis on noopt)
          (match sublis
            ((list* '&rest arg nil)
             (return (values acc arg)))
            ((list* '&rest _ _)
             (error "&rest should be in the last position!"))
            ((list* (list* arg _) _)    ; default value
             (collect arg into acc))
            ((list* arg _)
             (collect arg into acc)))
          (finally (return (values acc nil))))))

;; (remove-keywords '(a &optional x y &rest rest))
;; (A X Y)
;; REST
;; (remove-keywords '(a &optional (x 1) (y 2 y-p) &rest rest))
;; (A X Y)
;; REST
;; (remove-keywords '(a &rest rest &optional x y ))
;; --> error

(defmacro deftex (name args &body body)
  (assert (listp args))
  (multiple-value-bind (trueargs restvar) (remove-keywords args)
    `(defun ,name (,@args)
       (format t "~&\\~(~a~)~{{~a}~}~{{~a}~}"
               ',name (list ,@trueargs) ,restvar)
       ,@body)))

(deftex begin (environment &rest args))
(deftex end (environment))
(deftex hline () (terpri))
(deftex centering () (terpri))
(deftex relsize (size))
(deftex multicolumn (num alignment contents))
;; (multicolumn 1 "|c|" "ff")

(deftex bgroup ())
(deftex egroup ())
(deftex caption (msg))

;; row editing

(defun combine-columns (&rest strings)
  (reduce (lambda (s1 s2) (format nil "~a~%~a" s1 s2))
          (apply #'mapcar
                 (lambda (&rest cells)
                   (apply #'concatenate 'string cells))
                 (mapcar (lambda (str) (ppcre:split #\Newline str))
                         strings))))
;; (combine-rows
;;  "aaa|
;; aaa|
;; aaa|"
;;  "bbb|
;; bbb|
;; bbb|"
;;  "ccc|
;; ccc|
;; ccc|")

(defun r (&optional (thing " ") (s *standard-output*))
  "print a tex table row"
  (format s " ~a & ~%" thing))
(defun r* (&optional (thing " ") (s *standard-output*))
  "print a tex table row, without &. for integration with multicolumn"
  (format s " ~a ~%" thing))

(defun last-&-newline (str)
  (let ((other-than-last
         (reduce (lambda (s1 s2) (format nil "~a \\\\~%~a" s1 s2))
          (ppcre:split "&\\s*\\n"
                       str))))
    (subseq other-than-last
            0 (ppcre:scan "&\\s*$" other-than-last))))

;;; main

(defvar *db*)
(defun myprint ()
  (let ((*db* (associative-array 3)))
    (handler-case
        (iter (match (read *standard-input*)
                ((list* domain prob solver _ _ data)
                 (setf (aaref *db* domain prob solver) data))))
      (end-of-file (c)
        (let ((solvers (associative-array-dimension *db* 2))
              (*print-case* :downcase))
          (begin "table*")(princ "[h]")
          (centering)(bgroup)(relsize -2)
          (begin "tabular"
                 (concatenate 'string
                              "|c|" ;domain
                              "c|cc|c||" ;ff ff2 ff2 ff2 * 5 pairs
                              "c|cc|c||" ;ff ff2 ff2 ff2 * 5 pairs
                              "c|cc|c||" ;ff ff2 ff2 ff2 * 5 pairs
                              "c|cc|c||" ;ff ff2 ff2 ff2 * 5 pairs
                              "c|cc|c||" ;ff ff2 ff2 ff2 * 5 pairs
                              "cc|c|" ; fffd
                               ))
          (terpri)
          (princ
           (last-&-newline
            (combine-columns
             (with-output-to-string (*standard-output*)
               (hline)
               (r :domain)
               (r)
               (hline)
               (iter (for d in (associative-array-dimension *db* 0))
                     (r d)))                ;domain
             (base-column 'ff)
             (cap-column 'ff 'ff2)
             (base-column 'fd)
             (cap-column 'fd 'fd2)
             (base-column 'probe)
             (cap-column 'probe 'probe2)
             (base-column 'cea)
             (cap-column 'cea 'cea2)
             (base-column 'mv)
             (cap-column 'mv 'mv2)
             (cap-column 'fd 'fffd))))
          (princ "\\\\")
          (hline)
          (end "tabular")
          (egroup)
          (caption "Mockup of the BIG TABLE")
          (end "table*"))))))

(defun summary (ratios)
  (if ratios
      (format nil "~3,1f(~3,1f)"
              (mean ratios)
              (standard-deviation ratios))
      "-"))

(defun base-column (base)
  (with-output-to-string (*standard-output*)
    (r*)
    (r (rename-solver base))
    (r "\\#")
    (r*)
    (iter (for d in (associative-array-dimension *db* 0))
          (r (iter (for p in (associative-array-dimension *db* 1))
                   (counting (aaref *db* d p base)))))))

(defun cap-column (base cap)
  (combine-columns
   (with-output-to-string (*standard-output*)
     (r*)
     (multicolumn 2 "|c|" (rename-solver cap)) (r*)
     (r "\\#")
     (r*)
     (iter (for d in (associative-array-dimension *db* 0))
           (r (iter (for p in (associative-array-dimension *db* 1))
                    (counting (aaref *db* d p cap))))))
   (with-output-to-string (*standard-output*)
     (r*)
     (r)
     (r "pp/wall")
     (r*)
     (iter (for d in (associative-array-dimension *db* 0))
           (for ratios =
                (iter (for p in (associative-array-dimension *db* 1))
                      (match (aaref *db* d p cap)
                        ((list* 0 _)
                         (warn "skipped"))
                        ((list* elapsed preprocess _)
                         (collecting
                          (float (/ preprocess elapsed)))))))
           (r (summary ratios))))
   (with-output-to-string (*standard-output*)
     (r*)
     (r "cost")
     (r "base/cap")                   ;average of cost ratio
     (r*)
     (iter (for d in (associative-array-dimension *db* 0))
           (for ratios =
                (iter (for p in (associative-array-dimension *db* 1))
                      (multiple-value-match
                          (values (aaref *db* d p base)
                                  (aaref *db* d p cap))
                        (((list* _ _ _ _ c1 _)
                          (list* _ _ _ _ c2 _))
                         (collecting
                          (float (/ c1 c2)))))))
           (r (summary ratios))))))

(myprint ;; (parse-integer (second sb-ext:*posix-argv*))
         )

(terpri)
