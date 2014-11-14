#! /usr/local/bin/sbcl --script

(load
 (merge-pathnames
             "quicklisp/setup"
             (user-homedir-pathname)))

(with-open-file (*standard-output* "/dev/null"
                                   :direction :output
                                   :if-exists :overwrite)
  (ql:quickload :iterate) (use-package :iterate)
  (ql:quickload :optima)  (use-package :optima)
  (ql:quickload :osicat) ; (use-package :osicat)
  (ql:quickload :cl-ppcre) ; (use-package :osicat)
  (ql:quickload :alexandria) (use-package :alexandria))


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

;;; assoc-array

(defstruct (associative-array (:constructor %associative-array))
  (dimensions 1 :type fixnum :read-only t)
  (%tables (vector (make-hash-table :test 'eq))
           :type (array hash-table 1)
           :read-only t) 
  (%array (make-array 0 :adjustable t :fill-pointer 0)
          :type (array t *)
          :read-only t))


(defun associative-array (dimensions)
  (%associative-array :dimensions dimensions
                      :%tables (iter (with tables = (make-array dimensions))
                                     (for i below dimensions)
                                     (setf (aref tables i) (make-hash-table :test 'eq))
                                     (finally (return tables)))
                      :%array (make-array (make-list dimensions :initial-element 0)
                                          :adjustable t)))

(defun aaref (associative-array &rest subscripts)
  (apply #'aref
         (associative-array-%array associative-array)
         (map 'list #'gethash
              subscripts
              (associative-array-%tables associative-array))))

(defun (setf aaref) (new-value associative-array &rest subscripts)
  (with-accessors ((%a associative-array-%array)) associative-array
    (apply #'(setf aref)
           new-value
           %a
           (map 'list
                (lambda (thing hash i)
                  (or (gethash thing hash)
                      (let* ((dimensions (array-dimensions %a))
                             (old (array-dimension %a i)))
                        (incf (nth i dimensions))
                        (adjust-array %a dimensions :initial-element nil)
                        (setf (gethash thing hash) old))))
                subscripts
                (associative-array-%tables associative-array)
                (iota (associative-array-dimensions associative-array))))))

(defun associative-array-dimension (associative-array dimension)
  (hash-table-keys 
   (aref (associative-array-%tables associative-array) dimension)))

;; (defparameter a (associative-array 2))
;; (setf (aaref a :one :one) 1)
;; (setf (aaref a :one :two) 2)
;; (setf (aaref a :three :two) 6)
;; (setf (aaref a :four :two) 8)
;; (associative-array-dimension a 0)
;; (associative-array-dimension a 1)


;;; gnuplot

(defun gp-quote (value)
  (match value
    ((type string) (format nil "\"~a\"" value))
    (nil "")
    ((type list)
     (reduce (lambda (str val)
               (format nil "~a ~a"
                       str (gp-quote val)))
             value))
    (_ value)))

(defun gp-map-args (args fn)
  (iter (for keyword in args by #'cddr)
        (for value in (cdr args) by #'cddr)
        (funcall fn keyword value)))

(defun gp-setup (&rest args
                 &key
                   (terminal :pdf terminal-p)
                   (output nil output-p)
                   &allow-other-keys)
  (let ((*print-case* :downcase))
    (unless terminal-p
      (format t "~&set ~a ~a" :terminal (gp-quote terminal)))
    (unless output-p
      (format t "~&set ~a ~a" :output (gp-quote output)))
    (gp-map-args args
                 (lambda (key val)
                   (format t "~&set ~a ~a"
                           key (gp-quote val))))))


(defvar *data-functions*)
(defmacro with-plots (&body body)
  `(let (*data-functions*)
     (format t "~&plot")
     ,@body
     (dolist (fn (nreverse *data-functions*))
       (funcall fn))))

(defun plot (data-producing-fn &rest args
             &key &allow-other-keys)
  (let ((*print-case* :downcase))
    (format t "~@[,~] '-'" *data-functions*)
    (gp-map-args
     args
     (lambda (&rest args)
       (match args
         ((list :using (and val (type list)))
          (format t " using ~{~a~^:~}" val))
         ((list :continue _)
          ) ; do nothing
         ((list key val)
          (format t " ~a ~a"
                  key (gp-quote val))))))
    (push
     (lambda ()
       (funcall data-producing-fn)
       (format t "~&end"))
     *data-functions*)))

(defun nop ())
(defun func-plot (string &rest args
                  &key &allow-other-keys)
  (let ((*print-case* :downcase))
    (format t "~@[,~] ~a" *data-functions* string)
    (gp-map-args
     args
     (lambda (&rest args)
       (match args
         ((list :using (and val (type list)))
          (format t " using ~{~a~^:~}" val))
         ((list :continue _)
          ) ; do nothing
         ((list key val)
          (format t " ~a ~a"
                  key (gp-quote val))))))
    (push #'nop *data-functions*)))
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
          (gp-setup :xlabel (format nil "cost ~a" (first solvers))
                    :ylabel (format nil "cost ~a" (second solvers))
                    :output (format nil "~a-~a.pdf"
                                    (first solvers)
                                    (second solvers))
                    :key '(:bottom :right :font "Times New Roman, 6")
                    :pointsize "0.4px")
          (with-plots
            (func-plot "x")
            (mapc (lambda (domain)
                    (plot (lambda ()
                   ;; compute max range
                   (let ((max/domain
                          (iter (for prob in (sort (associative-array-dimension db 1)
                                                   #'string<))
                                (maximizing
                                 (iter (for solver in solvers)
                                       (maximizing
                                        (match (aaref db domain prob solver)
                                          ((list* _ _ _ _ cost _)
                                           cost)
                                          (_ -1))))))))
                     (iter (for prob in (sort (associative-array-dimension db 1)
                                              #'string<))
                           (for costs =
                                (iter (for solver in solvers)
                                      (for data = (aaref db domain prob solver))
                                      (collect
                                       (float
                                          (/ (match data
                                               ((list* _ _ _ _ cost _)
                                                cost)
                                               (_ -1))
                                             max/domain)))))
                           (when (every #'plusp costs)
                                      (format t "~&~{~a~^ ~}" costs)))))
                          :using '(1 2)
                          :title (princ-to-string domain)))
                  (associative-array-dimension db 0))))))))


(myprint (parse-integer (second sb-ext:*posix-argv*)))

(terpri)
