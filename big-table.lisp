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
  (ql:quickload :iterate)
  (ql:quickload :optima)
  (ql:quickload :osicat)
  (ql:quickload :cl-ppcre)
  (ql:quickload :alexandria)
  (ql:quickload :associative-array))

(defpackage :bigtable
  (:use :cl :iterate :optima :alexandria :associative-array))
(in-package :bigtable)

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
    ;; rather old (aaai15)
    ;; (barman 'barman-l)
    ;; (cell-assembly-noneg-nocost 'assembly-mixed)
    ;; (elev-lessfloors 'elev-9floors)
    ;; (elev-20floor 'elev-20floors)       ; plural
    ;; (elev-40floor 'elev-40floors)
    ;; (gripper 'gripper-l)
    ;; (openstacks-nocost 'openstacks-l)
    ;; (rovers 'rovers-l)
    ;; (satellite-typed2 'satellite-l)
    ;; (barman 'barman-l)
    ;; (woodworking-sat11-nocost 'woodworking-l)
    ;; (pipesworld-notankage 'pipesworld-nt)
    ;; (pipesworld-tankage 'pipesworld)
    ;; (tpp 'tpp-l)
    ;; (miconic 'miconic-l)

    (barman 'barman-large)
    (assembly-mixed 'assembly-mixed-large)
    (elevators 'elevators-large)
    (openstacks 'openstacks-large)
    (rovers 'rovers-large)
    (woodworking 'woodworking-large)
    (transport 'transport-large)
    (tidybot 'tidybot-large)
    (visitall 'visitall-large)
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
    (ff2tl '\\fffftl)
    
    (fd '\\fd)
    (fd2 '\\fdfd)
    (fd2ncp '\\fdfdncp)
    (fd2ncy '\\fdfdncy)
    (fd2tl '\\fdfdtl)
    
    (cea '\\cea)
    (cea2 '\\cc)
    (cea2ncp '\\ccncp)
    (cea2ncy '\\ccncy)
    (cea2tl '\\cctl)

    (fffd '\\fffd)
    (fffdncp '\\fffdncp)
    (fffdncy '\\fffdncy)
    (fffdtl '\\fffdtl)
    
    (mv '\\mv)
    (mv2 '\\mvmv)
    ;; (mv '\\mv2)
    ;; (mv2 '\\mv2mv2)
    ;; (mvr1 '\\mv)
    ;; (mvr12 '\\mvmv)
    (probe '\\pr)
    (probe2 '\\prpr)
    (probe2ncp '\\prprncp)
    (probe2ncy '\\prprncy)
    (probe2tl '\\prprtl)
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
(deftex label (label))

;; (deftex rotatebox (degree contents))
(DEFUN ROTATEBOX (DEGREE CONTENTS)
  (FORMAT T "~&\\~(~a~)[origin=c]~{{~a}~}~{{~a}~}" 'ROTATEBOX (LIST DEGREE CONTENTS) NIL))
;;; row editing

(defun combine-columns (&rest strings)
  (reduce (lambda (s1 s2) (format nil "~a~%~a" s1 s2))
          (apply #'mapcar
                 (lambda (&rest cells)
                   (apply #'concatenate 'string cells))
                 (mapcar (lambda (str) (ppcre:split #\Newline str))
                         (remove nil strings)))))
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
(defvar % nil)
(defvar *argv* (copy-list (cdr sb-ext:*posix-argv*)))
(defun myprint (&aux (mode (or (pop *argv*) "ipc"))
                  (title (or (pop *argv*) mode)))
  (let ((*db* (associative-array 3)))
    (handler-case
        (iter (match (read *standard-input*)
                ((list* domain prob solver _ _ data)
                 (setf (aaref *db* domain prob solver) data))))
      (end-of-file (c)
        (let ((solvers (associative-array-dimension *db* 2))
              (*print-case* :downcase))
          (begin "table*")(princ "[htb]")
          (centering)(bgroup)(relsize -2)
          (funcall
           (symbol-function
            (read-from-string mode)))
          (egroup)
          (caption title)
          (label (format nil "tab:~a" mode))
          (end "table*"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ipc ()
  (begin "tabular" "|c|*{4}{c|ccc|c||}ccc|c|")
  (terpri)
  (princ
   (last-&-newline
    (combine-columns
     (first-column)
     (base-column 'ff)
     (cap-column 'ff2)
     (cost-column 'ff 'ff2)
     (base-column 'fd)
     (cap-column 'fd2)
     (cost-column 'fd 'fd2)
     (base-column 'probe)
     (cap-column 'probe2)
     (cost-column 'probe 'probe2)
     (base-column 'cea)
     (cap-column 'cea2)
     (cost-column 'cea 'cea2)
     (cap-column 'fffd)
     (cost-column 'fd 'fffd))))
  (princ "\\\\")
  (hline)
  (end "tabular"))

(defun large ()
  (begin "tabular" "|c|*{4}{c|ccc|c|c||}ccc|c|c|")
  (terpri)
  (princ
   (last-&-newline
    (combine-columns
     (first-column)
     (base-column 'ff)
     (cap-column 'ff2)
     (tl-column 'ff2tl)
     (cost-column 'ff 'ff2)
     (base-column 'fd)
     (cap-column 'fd2)
     (tl-column 'fd2tl)
     (cost-column 'fd 'fd2)
     (base-column 'probe)
     (cap-column 'probe2)
     (tl-column 'probe2tl)
     (cost-column 'probe 'probe2)
     (base-column 'cea)
     (cap-column 'cea2)
     (tl-column 'cea2tl)
     (cost-column 'cea 'cea2)
     (cap-column 'fffd)
     (tl-column 'fffdtl)
     (cost-column 'fd 'fffd))))
  (princ "\\\\")
  (hline)
  (end "tabular"))

;; (defun appendix ()
;;   (begin "tabular" "|c|*{4}{c|ccc|c||}ccc|c|")
;;   (terpri)
;;   (princ
;;    (last-&-newline
;;     (combine-columns
;;      (first-column)
;;      (base-column 'ff)
;;      (cap-column 'ff2)
;;      (base-column 'ff2tl)
;;      (cost-column 'ff 'ff2)
;;      (base-column 'fd)
;;      (cap-column 'fd2)
;;      (base-column 'fd2tl)
;;      (cost-column 'fd 'fd2)
;;      (base-column 'probe)
;;      (cap-column 'probe2)
;;      (base-column 'probe2tl)
;;      (cost-column 'probe 'probe2)
;;      (base-column 'cea)
;;      (cap-column 'cea2)
;;      (base-column 'cea2tl)
;;      (cost-column 'cea 'cea2)
;;      (cap-column 'fffd)
;;      (base-column 'fffdtl)
;;      (cost-column 'fd 'fffd))))
;;   (princ "\\\\")
;;   (hline)
;;   (end "tabular"))

(defun summary (ratios)
  (if ratios
      (format nil "~1,1f\\spm{}~1,1f"
              (mean ratios)
              (standard-deviation ratios))
      "-"))


(defun problems ()
  (associative-array-dimension *db* 1))
(defun domains ()
  (sort (associative-array-dimension *db* 0) #'string<))
(defun %# (count all)
  (if % (floor (* 100 (/ count all))) count))

(defun first-column ()
  (with-output-to-string (*standard-output*)
    (hline)
    (r "\\spc{Domain\\_{\\relsize{-1}$c(x)$:plan cost}}")
    (r "{\\relsize{-1}\\spc{\\#:coverage, ave.:average,\\_sd:standard deviation}}")
    (hline)
    (iter (for d in (domains))
          (r (format nil "{\\relsize{-1}~a(~a)}"
                     (rename-domain d)
                     (max-problem-number d)))) ;domain
    (hline)
    (r (if % "overall" "sum"))))

;;;; coverage

(defun show-coverage (solver column &optional (rotate 0))
  (with-output-to-string (*standard-output*)
    (r*)
    (multicolumn column "|c|"
                 (with-output-to-string (*standard-output*)
                   (rotatebox rotate (rename-solver solver)))) (r)
    (r (if % "\\%" "\\#"))
    (r*)
    (show-coverage/domain solver)
    (r*)
    (show-coverage/overall solver)))

(defun show-coverage/domain (solver)
  (iter (for d in (domains))
        (r (iter nil
                 (for p in (problems))
                 (counting
                  (match (aaref *db* d p solver)
                    ((list* _ _ _ _ (satisfies plusp) _) t))
                  into count)
                 (counting p into all)
                 (finally (return (%# count all)))))))

(defun show-coverage/overall (solver)
  (r (iter outer
           (for d in (domains))
           (iter (for p in (problems))
                 (in outer
                     (counting
                      (match (aaref *db* d p solver)
                        ((list* _ _ _ _ (satisfies plusp) _) t))
                      into count)
                     (counting p into all)))
           (finally (return-from outer (%# count all))))))


;;;; main columns

(defun base-column (base)
  (show-coverage base 1 -90))
(defun tl-column (base)
  (show-coverage base 1))
(defun cap-column (cap)
  (combine-columns
   ;; coverage %
   (show-coverage cap 3)
   (length-ratio cap)
   (preprocessing-success+failure cap)))

   ;; (if full
   ;;     (preprocessing-success/failure base cap))
   ;; (when full
   ;;   (length-ratio cap))

;;;; preprocessing

(defun preprocessing-success+failure (cap)
  (with-output-to-string (*standard-output*)
    (r*)
    (r*)
    (r "{\\relsize{-1}\\spc{pp/wall\\_ave.\\spm{}sd}}")
    (r*)
    (iter (for d in (domains))
          (for ratios =
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* elapsed preprocess _)
                        (collecting
                         (float (/ preprocess
                                   (if (minusp elapsed)
                                       1800
                                       elapsed))))))))
          (r (summary ratios)))
    (r*)
    (r (summary
        (iter (for d in (domains))
              (appending
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* elapsed preprocess _)
                        (collecting
                         (float (/ preprocess
                                   (if (minusp elapsed)
                                       1800
                                       elapsed)))))))))))))

(defun preprocessing-success/failure (base cap)
  (with-output-to-string (*standard-output*)
    (r*)
    (r*)
    (r "{\\relsize{-1}\\spc{pp/wall\\_ave.\\spm{}sd\\_solved/failed}}")
    (r*)
    (iter (for d in (domains))
          (for ratios =
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* elapsed preprocess _ _ (satisfies plusp) _)
                        (collecting
                         (float (/ preprocess elapsed)))))))
          (for fratios =
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* elapsed preprocess _ _ (satisfies minusp) _)
                        (collecting
                         (float (/ preprocess elapsed)))))))
          (r (format nil "~a/~a" (summary ratios) (summary fratios))))
    (r*)
    (r (format
        nil "~a/~a"
        (summary
         (iter (for d in (domains))
               (appending
                (iter (for p in (problems))
                      (match (aaref *db* d p cap)
                        ((list* elapsed preprocess _ _ (satisfies plusp) _)
                         (collecting
                          (float (/ preprocess elapsed)))))))))
        (summary
         (iter (for d in (domains))
               (appending
                (iter (for p in (problems))
                      (match (aaref *db* d p cap)
                        ((list* elapsed preprocess _ _ (satisfies minusp) _)
                         (collecting
                          (float (/ preprocess elapsed)))))))))))))
(defun preprocessing-success-row (base cap)
  (with-output-to-string (*standard-output*)
    (r*)
    (r*)
    (r "\\spc{pp[sec]/wall[sec]\\_ave.(sd)\\_solved/unsolved}")
    (r*)
    (iter (for d in (domains))
          (for ratios =
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* elapsed preprocess _ _ (satisfies plusp) _)
                        (collecting
                         (float (/ preprocess elapsed)))))))
          (r (summary ratios)))
    (r*)
    (r (summary
        (iter (for d in (domains))
              (appending
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* elapsed preprocess _ _ (satisfies plusp) _)
                        (collecting
                         (float (/ preprocess elapsed))))))))))))

(defun preprocessing-failure-row (base cap)
  (with-output-to-string (*standard-output*)
    (r*)
    (r*)
    (r "\\spc{pp/wall\\_(unsolved)}")
    (r*)
    (iter (for d in (domains))
          (for ratios =
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* elapsed preprocess _ _ (satisfies minusp) _)
                        (collecting
                         (float (/ preprocess elapsed)))))))
          (r (summary ratios)))
    (r*)
    (r (summary
        (iter (for d in (domains))
              (appending
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* elapsed preprocess _ _ (satisfies minusp) _)
                        (collecting
                         (float (/ preprocess elapsed))))))))))))



;;;; cost

(defun cost-column (base cap)
  (with-output-to-string (*standard-output*)
    (r*)
    (r (format nil
               "$\\frac{c\\left(\\mbox{\\relsize{-1}~a}\\right)}{c\\left(\\mbox{\\relsize{-1}~a}\\right)}$"
               (rename-solver cap)
               (rename-solver base)))
    (r "{\\relsize{-1}ave.\\spm{}sd}")
    (r*)
    (iter (for d in (domains))
          (for ratios =
               (iter (for p in (problems))
                     (multiple-value-match
                         (values (aaref *db* d p base)
                                 (aaref *db* d p cap))
                       (((list* _ _ _ _ (and c1 (satisfies plusp)) _)
                         (list* _ _ _ _ (and c2 (satisfies plusp)) _))
                        (collecting
                         (float (/ c2 c1)))))))
          (r (summary ratios)))
    (r*)
    (r (summary
        (iter (for d in (domains))
              (appending
               (iter (for p in (problems))
                     (multiple-value-match
                         (values (aaref *db* d p base)
                                 (aaref *db* d p cap))
                       (((list* _ _ _ _ (and c1 (satisfies plusp)) _)
                         (list* _ _ _ _ (and c2 (satisfies plusp)) _))
                        (collecting
                         (float (/ c2 c1))))))))))))



;;;; length

(defun length-ratio (cap)
  (with-output-to-string (*standard-output*)
    (r*)
    (r*)
    (r "{\\relsize{-1}\\spc{$l_e/l_p$\\_ave.\\spm{}sd}}")
    (r*)
    (iter (for d in (sort (domains)
                          #'string<))
          (for ratios =
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* _ _ _ _ _
                               (and length (satisfies plusp)) _
                               (and metalength (satisfies plusp)) _ _)
                        (collecting
                         (float (/ metalength length)))))))
          (r (summary ratios)))
    (r*)
    (r (summary
        (iter (for d in (domains))
              (appending
               (iter (for p in (problems))
                     (match (aaref *db* d p cap)
                       ((list* _ _ _ _ _
                               (and length (satisfies plusp)) _
                               (and metalength (satisfies plusp)) _ _)
                        (collecting
                         (float (/ metalength length))))))))))))

;;; main

(let ((dir (lastcar (pathname-directory *default-pathname-defaults*))))
  (print dir)
  (with-open-file (s (format nil "~~/repos/papers/aaai15/~a-table.tex" dir)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (let ((*standard-output* (make-broadcast-stream s *standard-output*)))
      (myprint))))

(terpri)
