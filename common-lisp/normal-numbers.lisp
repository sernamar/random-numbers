(ql:quickload :gsll)

(defpackage #:random-numbers
  (:use #:cl)
  (:import-from #:gsll
                #:make-random-number-generator
                #:+mt19937+
                #:sample))

(in-package #:random-numbers)

(defun generate-normal-random-numbers (&key (number-of-elements 1) (mean 0) (standard-deviation 1d0))
  (declare (type double-float standard-deviation))
  (let* ((*random-state* (make-random-state t)) ; necessary to get a new seed each time we run the standalone executable created using SBCL
         (seed (random (expt 2 32)))
         (rng (make-random-number-generator +mt19937+ seed)))
    (loop :repeat number-of-elements
          :collect (+ mean (sample rng :gaussian :sigma standard-deviation)))))

(defun main (&optional (samples 1))
  ;; parse arguments
  (let ((args (uiop:command-line-arguments)))
    (when args
      (setf samples (parse-integer (first args)))))  
  ;; generate random numbers
  (format t "~a~%" (generate-normal-random-numbers :number-of-elements samples)))

;;; To create an executable program using SBCL, use:
;; (sb-ext:save-lisp-and-die "normal-numbers" :toplevel #'main :executable t)
