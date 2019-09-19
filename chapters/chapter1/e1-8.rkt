#lang racket
; Exercise 1.8.
; Newton's method for cube roots is based on the fact that
; if y is an approximation to the cube root of x, then a better approximation is given by the value
; (x/y^2 + 2y) / 3
; Use this formula to implement a cube-root procedure analogous to the square-root procedure.

(define (qbrt-iter guess x)
  (if (qbrt-good-enough? guess x)
      guess
      (qbrt-iter (qbrt-improve guess x)
                 x)))

(define (qbrt-improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (qbrt-good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (my-qbrt x)
  (qbrt-iter 1.0 x))

(my-qbrt 27)