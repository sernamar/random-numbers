(ql:quickload :bordeaux-threads)

(defpackage #:random-numbers
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:join-thread))

(in-package #:random-numbers)

(defun initialize-array (array start end)
  (loop :for i :from start :below end
        :do (setf (aref array i) i))
  array)

(defun p-initialize-array (array &optional (number-of-threads 4))
  (let* ((length (length array))
         (elements-per-thread (floor (/ length number-of-threads))))
    (loop :for i :from 0 :below number-of-threads
          :for start = (* i elements-per-thread)
          :for end = (+ start elements-per-thread)
          :do (progn
                (when (= i (- number-of-threads 1))
                  (setf end length))
                ;(format t "start: ~a, end: ~a~%" start end)
                (bt:join-thread (bt:make-thread (lambda () (initialize-array array start end)))))))
  array)

(defun main (&optional (number-of-elements 1) (number-of-threads 4))
  ;; parse arguments
  (let ((args (uiop:command-line-arguments)))
    (when args
      (setf number-of-elements (parse-integer (first args)))
      (setf number-of-threads (parse-integer (second args)))))  
  ;; initialize-array
  (let ((array (make-array number-of-elements)))
    (p-initialize-array array number-of-threads)
    array))

;;; To create an executable program using SBCL, use:
;; (sb-ext:save-lisp-and-die "p-normal-numbers" :toplevel #'main :executable t)
