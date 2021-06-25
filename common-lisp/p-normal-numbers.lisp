(ql:quickload :bordeaux-threads)
(ql:quickload :cffi)
(ql:quickload :static-vectors)

(defpackage #:random-numbers
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:join-thread))

(in-package #:random-numbers)

;;; ----------------------- ;;;
;;; CFFI to the GSL library ;;;
;;; ----------------------- ;;;

(cffi:define-foreign-library libgsl
  (:unix "libgsl.so")
  (t (:default "libgsl")))

(cffi:use-foreign-library libgsl)

(cffi:defcvar ("gsl_rng_default" *gsl-rng-default*) :pointer)

(cffi:defcfun "gsl_rng_alloc" :pointer
  (gsl-rng-type :pointer))

(cffi:defcfun "gsl_rng_free" :void
  (gsl-rng :pointer))

(cffi:defcfun "gsl_rng_set" :void
  (gsl-rng :pointer)
  (seed :unsigned-long))

(cffi:defcfun "gsl_ran_gaussian" :double
  (gsl-rng :pointer)
  (standard-deviation :double))

;;; ----------------------- ;;;
;;; Generate random numbers ;;;
;;; ----------------------- ;;;

(defun make-gsl-rng ()
  (let ((r (gsl-rng-alloc *gsl-rng-default*))
        (seed (random (expt 2 32))))
    (gsl-rng-set r seed)
    r))

(defun free-gsl-rng (gsl-rng)
  (gsl-rng-free gsl-rng))

(defun generate-normal-random-number (gsl-rng &optional (mean 0) (standard-deviation 1.0d0))
  (+ mean (gsl-ran-gaussian gsl-rng standard-deviation)))

(defun initialize-array (array start end)
  (let ((gsl-rng (make-gsl-rng)))
    (loop :for i :from start :below end
          :do (setf (aref array i) (generate-normal-random-number gsl-rng)))
    (free-gsl-rng gsl-rng)
    array))

(defun initialize-array-in-parallel (array &optional (number-of-threads 4))
  (let* ((length (length array))
         (elements-per-thread (floor (/ length number-of-threads))))
    (loop :for i :from 0 :below number-of-threads
          :for start = (* i elements-per-thread)
          :for end = (+ start elements-per-thread)
          :do (progn
                (when (= i (- number-of-threads 1))
                  (setf end length))
                (bt:join-thread (bt:make-thread (lambda () (initialize-array array start end)))))))
  array)

(defun main (&optional (number-of-elements 1) (number-of-threads 4))
  ;; parse arguments
  (let ((args (uiop:command-line-arguments)))
    (when args
      (setf number-of-elements (parse-integer (first args)))
      (setf number-of-threads (parse-integer (second args)))))  
  ;; initialize-array
  (let ((array (static-vectors:make-static-vector number-of-elements :element-type '(double-float))))
    (initialize-array-in-parallel array number-of-threads)
    (static-vectors:free-static-vector array) ; free the static vector to avoid memory leaks
    'done))

;;; To create an executable program using SBCL, use:
;; (sb-ext:save-lisp-and-die "p-normal-numbers" :toplevel #'main :executable t)
