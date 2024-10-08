(defpackage #:nonogram-solver/tests/test-nonograms
  (:use #:common-lisp)
  (:export #:*test-nonograms*))

(in-package #:nonogram-solver/tests/test-nonograms)

(defparameter *test-nonograms*
  (vector
   (cons (vector (vector '#(1) '#(2) '#(3) '#(2 1) '#(4))
                 (vector '#(1 1) '#(4) '#(1 1 1) '#(3) '#(1)))
         (vector (vector 0 0 1 0 0)
                 (vector 1 1 0 0 0)
                 (vector 0 1 1 1 0)
                 (vector 1 1 0 1 0)
                 (vector 0 1 1 1 1)))
   
   (cons (vector (vector '#(3) '#(2) '#(2 2) '#(1) '#(1 2))
                 (vector '#(1) '#(3) '#(1) '#(3 1) '#(3 1)))
         (vector (vector 0 0 1 1 1)
                 (vector 0 0 0 1 1)
                 (vector 1 1 0 1 1)
                 (vector 0 1 0 0 0)
                 (vector 0 1 0 1 1)))
   
   (cons (vector (vector '#(2) '#(3 1) '#(1 2) '#(3) '#(1))
                 (vector '#(3) '#(2) '#(1 1) '#(2) '#(4)))
         (vector (vector 1 1 0 0 0)
                 (vector 1 1 1 0 1)
                 (vector 1 0 0 1 1)
                 (vector 0 0 1 1 1)
                 (vector 0 0 0 0 1)))
   
   (cons (vector (vector '#(1 1) '#(3) '#(3) '#(3) '#(2))
                 (vector '#(1) '#(2) '#(3) '#(1 3) '#(3)))
         (vector (vector 0 1 0 1 0)
                 (vector 1 1 1 0 0)
                 (vector 0 0 1 1 1)
                 (vector 0 0 1 1 1)
                 (vector 0 0 0 1 1)))

   (cons (vector (vector '#(3) '#(1 1) '#(2) '#(3) '#(2 1))
                 (vector '#(1 1) '#(1 1) '#(4) '#(3) '#(1 1)))
         (vector (vector 1 1 1 0 0)
                 (vector 0 0 1 0 1)
                 (vector 0 0 1 1 0)
                 (vector 0 0 1 1 1)
                 (vector 1 1 0 1 0)))
   
   (cons (vector (vector '#(4) '#(3) '#(1) '#(2) '#(3))
                 (vector '#(1) '#(1) '#(3 1) '#(2 2) '#(1 2)))
         (vector (vector 1 1 1 1 0)
                 (vector 0 0 1 1 1)
                 (vector 0 0 1 0 0)
                 (vector 0 0 0 1 1)
                 (vector 0 0 1 1 1)))
   
   (cons (vector (vector '#(3) '#(3) '#(3) '#(2) '#(2))
                 (vector '#(3) '#(4) '#(3) '#(2) '#(1)))
         (vector (vector 0 0 1 1 1)
                 (vector 0 1 1 1 0)
                 (vector 1 1 1 0 0)
                 (vector 1 1 0 0 0)
                 (vector 1 1 0 0 0)))
   
   (cons (vector (vector '#(3) '#(1 1) '#(2) '#(3) '#(3))
                 (vector '#(1) '#(2) '#(2 2) '#(1 1 1) '#(3)))
         (vector (vector 0 0 1 1 1)
                 (vector 0 0 1 0 1)
                 (vector 0 0 0 1 1)
                 (vector 1 1 1 0 0)
                 (vector 0 1 1 1 0)))
   
   (cons (vector (vector '#(4) '#(1 2) '#(1 1) '#(2) '#(2))
                 (vector '#(3) '#(2 2) '#(1) '#(2) '#(3)))
         (vector (vector 0 1 1 1 1)
                 (vector 0 1 0 1 1)
                 (vector 1 0 0 0 1)
                 (vector 1 1 0 0 0)
                 (vector 1 1 0 0 0)))
   
   (cons (vector (vector '#(1) '#(2) '#(4) '#(3) '#(3))
                 (vector '#(1 1) '#(1 1) '#(3) '#(3) '#(3)))
         (vector (vector 1 0 0 0 0)
                 (vector 0 0 0 1 1)
                 (vector 0 1 1 1 1)
                 (vector 0 0 1 1 1)
                 (vector 1 1 1 0 0)))
   
   (cons (vector (vector '#(2) '#(3 1) '#(3) '#(1 1) '#(2))
                 (vector '#(2) '#(3) '#(4) '#(1 1) '#(1 1)))
         (vector (vector 1 1 0 0 0)
                 (vector 1 1 1 0 1)
                 (vector 0 1 1 1 0)
                 (vector 0 0 1 0 1)
                 (vector 0 0 1 1 0)))
   
   (cons (vector (vector '#(3 2) '#(1 1 1 1) '#(1 2 1 2) '#(1 2 1 1 3) '#(1 1 2 1) '#(2 3 1 2) '#(9 3) '#(2 3) '#(1 2) '#(1 1 1 1) '#(1 4 1) '#(1 2 2 2) '#(1 1 1 1 1 1 2) '#(2 1 1 2 1 1) '#(3 4 3 1))
                 (vector '#(4 3) '#(1 6 2) '#(1 2 2 1 1) '#(1 2 2 1 2) '#(3 2 3) '#(2 1 3) '#(1 1 1) '#(2 1 4 1) '#(1 1 1 1 2) '#(1 4 2) '#(1 1 2 1) '#(2 7 1) '#(2 1 1 2) '#(1 2 1) '#(3 3)))
         (vector (vector 0 0 1 1 1 0 0 0 0 1 1 0 0 0 0)
                 (vector 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0)
                 (vector 1 0 0 1 1 0 0 1 0 0 0 1 1 0 0)
                 (vector 1 0 1 1 0 0 0 1 0 0 1 0 1 1 1)
                 (vector 1 0 1 0 0 0 0 0 1 1 0 0 0 0 1)
                 (vector 1 1 0 1 1 1 0 0 0 1 0 0 0 1 1)
                 (vector 0 1 1 1 1 1 1 1 1 1 0 1 1 1 0)
                 (vector 0 1 1 0 0 0 0 0 0 1 1 1 0 0 0)
                 (vector 0 1 0 0 0 0 0 0 0 0 1 1 0 0 0)
                 (vector 0 1 0 0 1 0 0 1 0 0 0 1 0 0 0)
                 (vector 0 1 0 0 1 1 1 1 0 0 0 1 0 0 0)
                 (vector 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0)
                 (vector 1 0 1 0 0 1 0 1 0 1 0 1 0 1 1)
                 (vector 1 1 0 1 0 1 0 0 1 1 0 0 1 0 1)
                 (vector 0 1 1 1 0 1 1 1 1 0 1 1 1 0 1)))
   
   (cons (vector (vector '#(4) '#(6) '#(2 2) '#(2 2) '#(2) '#(2) '#(2) '#(2) '#() '#(2) '#(2))
                 (vector '#(3) '#(4) '#(2 2 2) '#(2 4 2) '#(6) '#(3)))
         (vector (vector 0 1 1 1 1 0)
                 (vector 1 1 1 1 1 1)
                 (vector 1 1 0 0 1 1)
                 (vector 1 1 0 0 1 1)
                 (vector 0 0 0 1 1 0)
                 (vector 0 0 0 1 1 0)
                 (vector 0 0 1 1 0 0)
                 (vector 0 0 1 1 0 0)
                 (vector 0 0 0 0 0 0)
                 (vector 0 0 1 1 0 0)
                 (vector 0 0 1 1 0 0)))
   
   (cons (vector (vector '#(2 1 1) '#(3 4 2) '#(4 4 2) '#(8 3) '#(7 2 2) '#(7 5) '#(9 4) '#(8 2 3) '#(7 1 1) '#(6 2) '#(5 3) '#(3 6 3) '#(2 9 2) '#(1 8) '#(1 6 1) '#(3 1 6) '#(5 5) '#(1 3 8) '#(1 2 6 1) '#(1 1 1 3 2))
                 (vector '#(1 1 3) '#(3 2 1 3) '#(2 2) '#(3 6 3) '#(3 8 2) '#(15) '#(8 5) '#(15) '#(7 1 4 2) '#(7 9) '#(6 4 2) '#(2 1 5 4) '#(6 4) '#(2 6) '#(2 5) '#(5 2 1) '#(6 1) '#(3 1) '#(1 4 2 1) '#(2 2 2 2)))
         (vector (vector 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1) (vector 0 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0 0 0 1 1) (vector 1 1 1 1 0 0 0 0 1 1 1 1 0 0 1 1 0 0 0 0) (vector 0 0 0 1 1 1 1 1 1 1 1 0 0 0 1 1 1 0 0 0) (vector 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 1 1 0 1 1) (vector 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1) (vector 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0) (vector 0 1 1 1 1 1 1 1 1 0 0 0 1 1 0 0 1 1 1 0) (vector 0 1 1 1 1 1 1 1 0 0 0 0 1 0 0 0 1 0 0 0) (vector 0 0 0 1 1 1 1 1 1 0 0 1 1 0 0 0 0 0 0 0) (vector 0 0 0 1 1 1 1 1 0 0 1 1 1 0 0 0 0 0 0 0) (vector 0 0 0 1 1 1 0 1 1 1 1 1 1 0 0 0 0 1 1 1) (vector 1 1 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 1) (vector 1 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0) (vector 1 0 0 0 1 1 1 1 1 1 0 0 0 1 0 0 0 0 0 0) (vector 0 0 0 0 0 1 1 1 0 1 0 1 1 1 1 1 1 0 0 0) (vector 0 0 0 0 0 1 1 1 1 1 0 1 1 1 1 1 0 0 0 0) (vector 0 1 0 1 1 1 0 1 1 1 1 1 1 1 1 0 0 0 0 0) (vector 0 1 0 1 1 0 0 0 0 1 1 1 1 1 1 0 0 0 0 1) (vector 0 1 0 1 0 0 0 0 0 1 0 0 0 1 1 1 0 0 1 1)))
   (cons (vector (vector '#(3) '#(4 1) '#(3 1) '#(7) '#(1 1 1) '#(2 1 1) '#(8) '#(1 4) '#(2 5) '#(1 3))
                 (vector '#(3 2) '#(4 1) '#(4 2) '#(1 5) '#(1 1) '#(4 1) '#(3 3) '#(7) '#(4) '#(4)))
         (vector (vector 1 1 1 0 0 0 0 0 0 0) (vector 1 1 1 1 0 0 1 0 0 0) (vector 1 1 1 0 0 0 1 0 0 0) (vector 0 1 1 1 1 1 1 1 0 0) (vector 0 0 0 1 0 1 0 1 0 0) (vector 0 0 1 1 0 1 0 1 0 0) (vector 0 0 1 1 1 1 1 1 1 1) (vector 0 0 0 1 0 0 1 1 1 1) (vector 1 1 0 0 0 1 1 1 1 1) (vector 1 0 0 0 0 0 0 1 1 1)))
   (cons (vector (vector '#(1 6) '#(1 6) '#(4) '#(1 1) '#(1) '#(5) '#(2 4) '#(4 3) '#(3 2) '#(5 1))
                 (vector '#(2 4) '#(4) '#(3) '#(2 1 1) '#(3 2 1) '#(4 1) '#(3 2) '#(2 3) '#(2 4) '#(2 4)))
         (vector (vector 1 0 0 0 1 1 1 1 1 1) (vector 1 0 0 0 1 1 1 1 1 1) (vector 0 0 0 1 1 1 1 0 0 0) (vector 0 0 0 1 0 1 0 0 0 0) (vector 0 0 0 0 1 0 0 0 0 0) (vector 0 0 0 0 1 1 1 1 1 0) (vector 1 1 0 0 0 0 1 1 1 1) (vector 1 1 1 1 0 0 0 1 1 1) (vector 1 1 1 0 0 0 0 0 1 1) (vector 1 1 1 1 1 0 0 0 0 1)))
   (cons (vector (vector '#(1 1 1 2) '#(3) '#(2) '#(6) '#(7) '#(4 3) '#(4) '#(4) '#(4 1) '#(5 2))
                 (vector '#(1 2) '#(3) '#(7) '#(7) '#(2 2 3) '#(5 3) '#(6 1) '#(3) '#(1 1) '#(1 2)))
         (vector (vector 1 0 0 0 1 0 1 0 1 1) (vector 0 0 0 0 1 1 1 0 0 0) (vector 0 0 0 0 0 1 1 0 0 0) (vector 0 0 1 1 1 1 1 1 0 0) (vector 0 1 1 1 1 1 1 1 0 0) (vector 1 1 1 1 0 1 1 1 0 0) (vector 1 1 1 1 0 0 0 0 0 0) (vector 0 0 1 1 1 1 0 0 0 0) (vector 0 0 1 1 1 1 0 0 0 1) (vector 0 0 1 1 1 1 1 0 1 1)))
   (cons (vector (vector '#(3 1) '#(3 1) '#(5) '#(1 1 1) '#(3 1 3) '#(4 5) '#(8 5) '#(6 5) '#(9 3) '#(8 2) '#(9) '#(8) '#(6 2) '#(5) '#(5))
                 (vector '#(1 3) '#(2 3) '#(7 3) '#(11) '#(11) '#(2 1 7) '#(2 6) '#(3 7) '#(1 5) '#(1 4) '#(7 2) '#(3 3) '#(5) '#(6) '#(7)))
         (vector (vector 0 0 0 0 0 1 1 1 0 0 0 1 0 0 0) (vector 0 0 0 0 0 1 1 1 0 0 0 1 0 0 0) (vector 0 0 0 0 0 0 0 1 1 1 1 1 0 0 0) (vector 0 0 0 0 0 1 0 0 0 0 1 0 0 0 1) (vector 0 0 1 1 1 0 0 0 0 0 1 0 1 1 1) (vector 0 1 1 1 1 0 0 0 0 0 1 1 1 1 1) (vector 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1) (vector 0 0 1 1 1 1 1 1 0 0 1 1 1 1 1) (vector 0 0 1 1 1 1 1 1 1 1 1 0 1 1 1) (vector 0 0 1 1 1 1 1 1 1 1 0 0 0 1 1) (vector 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0) (vector 0 0 0 1 1 1 1 1 1 1 1 0 0 0 0) (vector 1 1 1 1 1 1 0 1 1 0 0 0 0 0 0) (vector 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0) (vector 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)))
   (cons (vector (vector '#(4 1 3) '#(4 1 3) '#(3 8) '#(3 7) '#(1 5) '#(1 1 5) '#(7) '#(1 7) '#(2 3) '#(3 1 4) '#(5 1 1 2) '#(8) '#(4) '#(2 2 3) '#(3 1 3))
                 (vector '#(6 1 2) '#(4 2) '#(4 1) '#(2 1) '#(3 2) '#(3 3 1) '#(2 3) '#(6 2) '#(9 2) '#(13) '#(8 4) '#(8 6) '#(4 2) '#(2 3) '#(1 3)))
         (vector (vector 1 1 1 1 0 0 0 0 1 0 1 1 1 0 0) (vector 1 1 1 1 0 0 0 0 1 0 1 1 1 0 0) (vector 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1) (vector 1 1 1 0 0 0 0 1 1 1 1 1 1 1 0) (vector 1 0 0 0 0 0 0 1 1 1 1 1 0 0 0) (vector 1 0 0 0 0 1 0 1 1 1 1 1 0 0 0) (vector 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0) (vector 1 0 0 0 0 1 1 1 1 1 1 1 0 0 0) (vector 0 0 0 0 0 0 0 0 1 1 0 0 1 1 1) (vector 0 0 0 0 1 1 1 0 0 1 0 1 1 1 1) (vector 0 0 0 1 1 1 1 1 0 1 0 1 0 1 1) (vector 0 0 0 0 1 1 1 1 1 1 1 1 0 0 0) (vector 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0) (vector 1 1 0 0 1 1 0 0 0 1 1 1 0 0 0) (vector 1 1 1 0 1 0 0 0 0 1 1 1 0 0 0)))
   (cons (vector (vector '#(3 1 1 1 4) '#(8 4 1) '#(6 3 3) '#(1 5 3 1 3 1) '#(5 8) '#(1 3 1) '#(2 6 1 1 3) '#(2 1 3 3 1) '#(9 2 5) '#(4 3 8) '#(9 7) '#(8 7) '#(3 4 6) '#(3 3 3 1) '#(3 2 3 3) '#(3 1 5 2) '#(3 1 6 2) '#(3 8 2) '#(2 9 7) '#(2 11 2) '#(5 1 1 1 1 4 2) '#(1 5 5 2) '#(5 3 4) '#(4 2 4) '#(3 1 4))
                 (vector '#(4 2 4) '#(3 3 3) '#(5 3 2) '#(4 5 5) '#(4 4 6) '#(7 7 1 4) '#(1 2 1 1 7 2 3) '#(2 3 10 1) '#(6 6) '#(1 9 3) '#(4 5 3) '#(3 3 3 1) '#(2 12) '#(1 2 1 3 3) '#(1 1 4 8) '#(10 3 1) '#(1 10 1 2) '#(9 2) '#(4 11 1 3) '#(4 6 1 1 5) '#(4 3 1 3) '#(2 2 1 1 1) '#(1 1 1 1 1) '#(1 1 7) '#(3 1 8)))
         (vector (vector 1 1 1 0 0 1 0 1 0 1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0) (vector 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 1) (vector 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1 1) (vector 1 0 1 1 1 1 1 0 0 0 1 1 1 0 0 1 0 0 1 1 1 0 0 0 1) (vector 0 0 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0) (vector 0 0 0 0 0 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0) (vector 1 1 0 0 0 1 1 1 1 1 1 0 0 1 0 1 0 0 0 0 0 0 1 1 1) (vector 1 1 0 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 0 0 0 0 0) (vector 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 0 1 1 1 1 1 0 0) (vector 0 0 1 1 1 1 0 0 1 1 1 0 0 0 1 1 1 1 1 1 1 1 0 0 0) (vector 0 0 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0) (vector 0 0 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 0 0 0 0) (vector 0 0 0 0 0 1 1 1 0 1 1 1 1 0 1 1 1 1 1 1 0 0 0 0 0) (vector 0 0 0 0 0 1 1 1 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0 0 1) (vector 0 0 0 0 0 1 1 1 0 0 0 1 1 0 0 0 1 1 1 0 0 0 1 1 1) (vector 0 0 0 0 0 0 1 1 1 0 0 0 1 0 1 1 1 1 1 0 0 0 0 1 1) (vector 0 0 0 0 0 0 1 1 1 0 0 0 1 0 1 1 1 1 1 1 0 0 0 1 1) (vector 0 0 0 0 0 0 0 1 1 1 0 1 1 1 1 1 1 1 1 0 0 0 0 1 1) (vector 1 1 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1) (vector 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 1 1) (vector 1 1 1 1 1 0 0 0 1 0 1 0 1 0 1 0 0 0 1 1 1 1 0 1 1) (vector 1 0 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 0 0 1 1 0 0 0 0) (vector 0 0 0 1 1 1 1 1 0 0 0 0 1 1 1 0 0 0 1 1 1 1 0 0 0) (vector 0 0 0 1 1 1 1 0 0 0 0 0 1 1 0 0 1 1 1 1 0 0 0 0 0) (vector 0 0 0 1 1 1 0 0 0 0 0 1 0 0 0 0 1 1 1 1 0 0 0 0 0)))
   (cons (vector (vector '#(1 5) '#(1 2 1 2) '#(2 2) '#(2 4) '#(1 1) '#(2 2 3) '#(2 1 1 1) '#(1 1 1) '#(5 1) '#(8))
                 (vector '#(2 1 1) '#(1 2 1) '#(1 1 1 3) '#(2 2 1 2) '#(1 3) '#(1 2 2 1) '#(4 2) '#(1 1 1 1 1) '#(2 1 1 1) '#(2 3 1)))
         (vector (vector 0 0 0 1 0 1 1 1 1 1) (vector 1 0 1 1 0 0 1 0 1 1) (vector 1 1 0 0 0 1 1 0 0 0) (vector 0 0 1 1 0 1 1 1 1 0) (vector 0 0 0 1 0 0 0 0 0 1) (vector 0 1 1 0 1 1 0 1 1 1) (vector 1 1 0 1 0 1 0 0 0 1) (vector 0 0 1 0 1 0 0 1 0 0) (vector 1 1 1 1 1 0 1 0 0 0) (vector 0 0 1 1 1 1 1 1 1 1)))
   (cons (vector (vector '#(4 3 1 2) '#(6 1 2) '#(1 1 1 1 5) '#(2 1 3 1 1) '#(3 1 4 2) '#(1 1 1 2 1) '#(1 1 3 1) '#(2 2 3 2 1) '#(1 4 1 1) '#(2 2 1 1) '#(2 1 1 2) '#(1 2 2 1 1) '#(2 1 6) '#(2 1 2 1 1 1) '#(1 2 2 2))
                 (vector '#(1 1 3 5) '#(4 1 1 2) '#(2 1 2 1) '#(3 2 1 1 1 1) '#(2 1 2 1 1) '#(2 2 2 1 2) '#(3 3 1 2) '#(1 1 3 1) '#(1 3 2 1 1) '#(2 1 1 1 1) '#(4 3 3) '#(3 2 1 1 1) '#(3 4 1) '#(1 1 5 1 1) '#(1 1 1 2)))
         (vector (vector 0 0 1 1 1 1 0 1 1 1 0 1 0 1 1) (vector 1 1 1 1 1 1 0 0 0 1 0 1 1 0 0) (vector 0 1 0 1 0 0 1 0 1 0 1 1 1 1 1) (vector 1 1 0 0 1 0 1 1 1 0 1 0 1 0 0) (vector 0 1 1 1 0 0 1 0 1 1 1 1 0 1 1) (vector 1 0 0 1 0 1 0 0 0 0 1 1 0 1 0) (vector 1 0 0 0 0 1 0 1 1 1 0 0 0 1 0) (vector 1 1 0 1 1 0 1 1 1 0 1 1 0 1 0) (vector 0 0 1 0 1 1 1 1 0 0 1 0 0 1 0) (vector 0 0 1 1 0 1 1 0 0 0 1 0 1 0 0) (vector 1 1 0 0 1 0 0 1 0 0 0 1 1 0 0) (vector 1 0 1 1 0 1 1 0 1 0 0 0 1 0 0) (vector 1 1 0 0 1 0 0 0 0 1 1 1 1 1 1) (vector 1 1 0 1 0 1 1 0 1 0 1 0 0 0 1) (vector 1 0 0 0 0 1 1 0 0 1 1 0 1 1 0)))
   (cons (vector (vector '#(2 1 1 1 2 1) '#(3 1 1 1 3) '#(2 8 2 1) '#(5 6 2) '#(1 1 1 1 2 2 1) '#(1 2 1 1) '#(1 1 2 1 1 2 1 1) '#(1 1 2 1 2 2) '#(8 2 1) '#(1 2 1 4) '#(3 1 2 1 1 1) '#(3 1 1 1 1) '#(2 2 4 7) '#(2 4 1 1 5) '#(2 2 1 3 1) '#(4 1 3 2 2) '#(1 2 1 1 2 2) '#(4 2 2 1) '#(2 1 1 5 3 1) '#(1 1 7))
                 (vector '#(1 2 3 1 1) '#(1 1 5 2) '#(2 1 1 1 2 2) '#(2 2 4 1 4) '#(1 1 2 1 3 4) '#(1 1 1 2 2 2) '#(2 1 2 2 3) '#(1 1 2 1 2 1) '#(3 1 1 1 2 4) '#(1 3 1 2 1 3) '#(2 1 1 2 1) '#(3 1 2 2 1 1) '#(5 1 1) '#(4 1 6 3) '#(2 1 1 1 4) '#(1 2 2 3 1 2) '#(3 1 3 2 1) '#(2 1 1 2 2 1 1) '#(1 1 1 5 2) '#(1 3 3 1 1 1 1)))
         (vector (vector 0 0 1 1 0 1 0 1 0 1 0 0 0 0 0 0 1 1 0 1) (vector 0 0 1 1 1 0 0 0 1 0 0 1 0 1 0 0 1 1 1 0) (vector 1 1 0 0 0 0 1 1 1 1 1 1 1 1 0 1 1 0 0 1) (vector 0 0 1 1 1 1 1 0 1 1 1 1 1 1 0 0 0 0 1 1) (vector 0 1 0 1 0 0 0 1 0 1 0 0 1 1 0 0 1 1 0 1) (vector 0 0 0 0 1 0 1 1 0 0 0 0 1 0 1 0 0 0 0 0) (vector 1 0 1 0 1 1 0 0 1 0 0 0 1 0 1 1 0 1 0 1) (vector 1 0 0 1 0 0 0 0 0 0 1 1 0 1 0 1 1 0 1 1) (vector 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 1 1 0 1) (vector 0 0 0 1 0 1 1 0 0 0 0 1 0 0 1 1 1 1 0 0) (vector 0 0 1 1 1 0 0 0 1 0 1 1 0 1 0 1 0 0 0 1) (vector 1 1 1 0 1 0 0 0 0 1 0 0 0 1 0 0 0 0 1 0) (vector 1 1 0 1 1 0 1 1 1 1 0 0 1 1 1 1 1 1 1 0) (vector 1 1 0 0 0 1 1 1 1 0 0 1 0 1 0 1 1 1 1 1) (vector 0 1 1 0 1 1 0 0 0 0 0 1 0 1 1 1 0 0 1 0) (vector 0 1 1 1 1 0 1 0 1 1 1 0 1 1 0 0 0 1 1 0) (vector 1 0 0 1 1 0 1 0 1 0 1 1 0 0 1 1 0 0 0 0) (vector 0 0 0 1 1 1 1 0 1 1 0 0 0 1 1 0 0 0 0 1) (vector 1 1 0 1 0 1 0 1 1 1 1 1 0 1 1 1 0 0 1 0) (vector 0 1 0 0 0 0 0 0 0 1 0 0 0 1 1 1 1 1 1 1)))
   (cons (vector (vector '#(1 3 5) '#(2 2 1 2) '#(3 2 1 2) '#(1 1 4 1) '#(1 2 1 1 5) '#(1 1 1 2 3) '#(1 6 1 1) '#(1 4 1) '#(2 3 2) '#(4 3 1 1 1) '#(3 1 4 2) '#(1 1 2 1) '#(1 1 2 2 1) '#(1 3 3 2) '#(6 3))
                 (vector '#(1 1 1 2 1 1) '#(2 3 2) '#(2 2 2 1) '#(1 2 7) '#(1 2 1 2) '#(1 2 2 3 2) '#(1 2 3 1 2 1) '#(4 2 2 1) '#(1 1 4 1 2) '#(1 1 3 1 1 3) '#(1 1 3 3) '#(2 2 1) '#(1 1 3 2) '#(6 1 1 1) '#(1 1 5)))
         (vector (vector 1 0 0 0 1 1 1 0 0 1 1 1 1 1 0) (vector 0 0 1 1 0 0 0 1 1 0 0 1 0 1 1) (vector 1 1 1 0 0 0 1 1 0 1 0 0 1 1 0) (vector 0 1 0 1 0 1 1 1 1 0 0 0 0 1 0) (vector 1 0 1 1 0 1 0 1 0 1 1 1 1 1 0) (vector 0 0 1 0 1 0 1 0 1 1 0 1 1 1 0) (vector 1 0 0 0 1 1 1 1 1 1 0 0 1 0 1) (vector 1 0 0 0 0 1 1 1 1 0 0 1 0 0 0) (vector 0 0 0 1 1 0 0 0 1 1 1 0 1 1 0) (vector 1 1 1 1 0 1 1 1 0 0 1 0 1 0 1) (vector 0 1 1 1 0 1 0 1 1 1 1 0 0 1 1) (vector 0 1 0 1 0 1 1 0 0 0 0 0 0 0 1) (vector 1 0 0 1 0 0 1 1 0 1 1 0 0 0 1) (vector 0 1 0 1 1 1 0 0 1 1 1 0 0 1 1) (vector 0 1 1 1 1 1 1 0 1 1 1 0 0 0 0)))
   (cons (vector (vector '#(1 7 1 2 1 2) '#(1 2 3 2 1 1) '#(2 2 1 1 1 1) '#(2 3 1 2 2) '#(4 6 2 1) '#(3 2 3 2 1 1) '#(1 2 2 1 1 2 1) '#(1 2 1 1 1 2 2 1) '#(3 3 1 1 2) '#(1 1 2 1 1 1 3) '#(2 1 1 5 2) '#(1 4 1 3 2 1 1) '#(1 3 3 1 1) '#(1 3 2 1 2) '#(1 3 1 6 1))
                 (vector '#(2 1 1 6) '#(2 1) '#(1 1 2 2 2) '#(3 1 4 2 1) '#(2 2 1 3 1) '#(1 1 3 1 1) '#(3 2 1 1) '#(3 1 3 3) '#(2 3 3 2) '#(6 1 1) '#(2 3 2 1) '#(4 2 3) '#(1 2 1 1 2) '#(1 1 2 3 2) '#(1 1 1 2 1) '#(1 2 4 1 1) '#(3 2 2) '#(1 1 2 1) '#(1 2 3 1) '#(1 4 7)))
         (vector (vector 1 0 1 1 1 1 1 1 1 0 1 0 1 1 0 1 0 0 1 1) (vector 1 0 0 1 1 0 1 1 1 0 1 1 0 0 1 0 1 0 0 0) (vector 0 0 1 1 0 0 1 1 0 1 0 1 0 0 0 0 1 0 1 0) (vector 0 0 0 0 1 1 0 0 0 1 1 1 0 1 0 1 1 0 1 1) (vector 0 1 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 0 0 1) (vector 1 1 1 0 0 1 1 0 1 1 1 0 1 1 0 0 1 0 0 1) (vector 0 0 0 1 0 1 1 0 1 1 0 1 0 1 0 1 1 0 0 1) (vector 1 0 1 1 0 1 0 1 0 1 0 1 1 0 1 1 0 1 0 0) (vector 0 0 1 1 1 0 1 1 1 0 0 0 0 1 0 1 0 0 1 1) (vector 1 0 0 1 0 0 0 1 1 0 0 1 0 1 0 1 0 1 1 1) (vector 1 1 0 0 1 0 0 0 1 0 1 1 1 1 1 0 0 0 1 1) (vector 1 0 1 1 1 1 0 1 0 1 1 1 0 0 1 1 0 1 0 1) (vector 1 0 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 1 0 1) (vector 1 0 0 0 0 0 0 1 1 1 0 0 1 1 0 0 1 0 1 1) (vector 1 0 0 1 1 1 0 0 0 0 1 0 1 1 1 1 1 1 0 1)))
   (cons (vector (vector '#(1 2 3 3 2 1) '#(2 1 2 1 4 1) '#(1 1 1 3 1 1) '#(2 1 2 1 2 1 1) '#(1 1 1 2 1 4 1 1) '#(3 1 2 2 1) '#(3 1 1 2 1 3 2) '#(1 1 1 4 1 2) '#(1 4 3 3) '#(1 4 3 1) '#(4 6 2 1) '#(1 4 1 1 1 1) '#(1 2 2 1 1 1 1) '#(2 1 2 1 5) '#(3 1 2 1 1) '#(2 3 4 5) '#(3 2 6) '#(1 4 3 5 1) '#(1 1 3 1 1 1 2 1) '#(1 2 1 1 2 1))
                 (vector '#(2 1 1 1 1 1 4) '#(3 2 2 1 2 1) '#(1 1 1 4 2) '#(1 2 1 1 1 1 3) '#(2 2 2 1 1 1 1) '#(2 3 1 1 1 2) '#(1 1 3 1 1) '#(2 1 1 3 3 2) '#(5 1 6 1) '#(1 1 1 3 1 4) '#(3 4 3) '#(3 4 1 1) '#(1 1 2 2 4) '#(6 1 2 2 2) '#(9 1 2 4) '#(2 1 1 1 1 5 1) '#(2 4 2 2 1) '#(1 4 2 1) '#(2 1 1 1 1) '#(8 3 2 1)))
         (vector (vector 1 0 0 1 1 0 0 1 1 1 0 0 1 1 1 0 1 1 0 1) (vector 1 1 0 0 1 0 0 1 1 0 0 1 0 1 1 1 1 0 0 1) (vector 0 1 0 0 0 0 0 0 1 0 0 1 0 1 1 1 0 1 0 1) (vector 0 1 1 0 0 0 1 0 1 1 0 1 0 1 1 0 0 1 0 1) (vector 1 0 0 1 0 1 0 1 1 0 1 0 1 1 1 1 0 1 0 1) (vector 0 0 0 1 1 1 0 0 0 0 1 0 0 1 1 0 1 1 0 1) (vector 1 1 1 0 1 0 1 0 0 1 1 0 1 0 1 1 1 0 1 1) (vector 0 1 0 0 0 1 0 1 0 0 0 1 1 1 1 0 1 0 1 1) (vector 1 0 1 1 1 1 0 0 0 1 1 1 0 0 1 1 1 0 0 0) (vector 0 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 1 0) (vector 1 1 1 1 0 0 1 1 1 1 1 1 0 1 1 0 0 0 0 1) (vector 0 0 1 0 1 1 1 1 0 0 1 0 0 1 0 1 0 0 0 1) (vector 1 0 1 1 0 0 0 0 1 1 0 1 0 0 1 0 1 0 0 1) (vector 0 1 1 0 0 1 0 1 1 0 1 0 1 1 1 1 1 0 0 0) (vector 0 0 0 0 0 0 1 1 1 0 1 0 1 1 0 1 0 0 0 1) (vector 1 1 0 1 1 1 0 1 1 1 1 0 0 0 0 1 1 1 1 1) (vector 1 1 1 0 0 0 0 0 1 1 0 0 1 1 1 1 1 1 0 0) (vector 1 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 0 1 0) (vector 1 0 0 1 0 1 1 1 0 1 0 0 1 0 1 0 1 1 0 1) (vector 0 1 0 1 1 0 0 0 1 0 0 0 1 0 1 1 0 0 1 0)))))
