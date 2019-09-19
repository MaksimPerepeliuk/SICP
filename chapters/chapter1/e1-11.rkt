#lang sicp
;ex 1.11***************************
;recursion process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (f (- n 2))
         (f (- n 3)))))
(f 6)
;iterative process
(define (fn n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a b c) a b (- count 1))))
  (iter 2 1 0 n))
(fn 6)
