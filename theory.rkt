#lang racket
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

#|
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
|#

(define (abs x)
  (if (< x 0)
      (- x)
      x))
#|
(define (>= x y)
  (or (= x y) (> x y)))
|#


(define (>= x y)
  (not (< x y)))

;exercises

#|
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))
|#

(define (sum-of-more a b c)
  (cond ((and (> a b) (> b c)) (sum-of-squares a b))
        ((and (> b a) (> c a)) (sum-of-squares b c))
        ((and (> a b) (> c b)) (sum-of-squares a c))))

(sum-of-more 5 3 4)