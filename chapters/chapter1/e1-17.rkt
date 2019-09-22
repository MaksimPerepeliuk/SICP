#lang sicp

(define (fast-mul a b) 
  (cond ((= b 0) 0) 
        ((= b 1) a) 
        ((even? b) (square (fast-mul a (halve b)))) 
        (else (+ a (fast-mul a (- b 1))))))
