#lang sicp
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
        (else (sum-of-squares a c))))

;1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

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

