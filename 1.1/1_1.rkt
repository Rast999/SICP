#lang racket

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

; Ex 1.2
(/
 (+ 5
    4
    (- 2
       (- 3
          (+ 6
             (/ 4 5)))))
 (* 3
    (- 6 2)
    (- 2 7)))

; Ex 1.3

(define (max-sum-squares x y z)
  (cond
    ((and (< x y) (< x z)) (sum-of-squares y z))
    ((and (< y x) (< y z)) (sum-of-squares x z))
    (else (sum-of-squares x y))))

; Ex 1.4
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))


; Ex 1.11

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (f (- n 2))
         (f (- n 3)))))

(define (f-iter a b c count)
  (if (< count 3)
      c
      (f-iter b c (+ a b c) (- count 1))))

; 1.12


; TESTS

(define (PT row index)
  (cond ((= index 0) 1)
        ((or (< index 0) (> index row)) 0)
        (else (+ (PT (- row 1) (- index 1)) (PT (- row 1) index)))))

(max-sum-squares 1 2 3)
(max-sum-squares 3 2 1)
(max-sum-squares 3 1 2)
(f 10)
(f-iter 0 1 2 10)
(PT 3 1)
(PT 4 0)
(PT 4 1)
(PT 4 2)
(PT 4 3)
(PT 4 4)
