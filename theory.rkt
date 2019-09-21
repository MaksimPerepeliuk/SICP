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

;1.1.7 **************************************************

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

;*******************************************************

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

;1.1.8 procedure as abstraction type "black box"
(define (block-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;***********************************************************

;1.2
(define (my-factorial n)
  (if (= n 1)
      1
  (* n (my-factorial (- n 1)))))

;*********************

(define (my-factorial2 n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

;********************
;1.2.2 tree recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
;ex 1.11***************************
;recursion process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (f (- n 2))
         (f (- n 3)))))
;iterative process
(define (fn n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a b c) a b (- count 1))))
  (iter 3 2 1 n))

;*********************

;1.2.4
;recursive process
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
;iterative process
(define (i-expt b n)
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (- counter 1)
              (* b product))))
  (iter n 1))

;***************************
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;***************************

(define (i-fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(i-fast-expt 5 2)

;*****************************




