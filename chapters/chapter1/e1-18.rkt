#lang sicp

(define (fast-mul2 a b) 
  (fast-mul-iter a b 0))
(define (fast-mul-iter a b c) 
  (cond ((= b 0) c) 
        ((even? b) (fast-mul-iter (square a) (halve b) c)) 
        (else (fast-mul-iter a (- b 1) (+ c a)))))

