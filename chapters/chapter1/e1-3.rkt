#lang sicp

; excercise 1.3
; Define procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers

(define (sum-of-more a b c)
  (cond ((and (> a b) (> b c)) (sum-of-squares a b))
        ((and (> b a) (> c a)) (sum-of-squares b c))
        (else (sum-of-squares a c))))

(sum-of-more 5 3 4)


