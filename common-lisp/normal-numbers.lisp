(ql:quickload :gsll)

(defpackage #:random-numbers
  (:use #:cl)
  (:import-from #:gsll
                #:make-random-number-generator
                #:+mt19937+
                #:sample))

(in-package #:random-numbers)

(defun generate-random-gaussian-samples (number-of-samples &key (mean 0) (standard-deviation 1d0))
  (declare (type double-float standard-deviation))
  (let* ((seed (random (expt 2 32)))
         (rng (make-random-number-generator +mt19937+ seed)))
    (loop :repeat number-of-samples
          :collect (+ mean (sample rng :gaussian :sigma standard-deviation)))))
