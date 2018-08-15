#lang racket
;(require racket/include)
;(include "lists.rkt")


; ############# Helpers #############

(define nil '())

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (square x) (* x x))

; ############# Implementation #############

(define (even-fibs~ n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

; Examples from page 121

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

; Ex. 2.33
(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

; Ex. 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coef higher-terms) (+ this-coef (* x higher-terms)))
              0
              coefficient-sequence))

; Ex. 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(define (count-leaves-r t)
  (accumulate + 0 (map (lambda (x) (cond ((null? x) 0)
                                         ((pair? x) (count-leaves-r x))
                                         (else 1))) t)))

; Ex. 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Ex. 2.37
; Operations with matrixes and vectors
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col)) cols)) m)))
; ############# Testing #############
(filter even? (map fib (enumerate-interval 0 20)))
(even-fibs 20)

(map square (filter even? (list 1 2 3 4 5 6 7 8 9)))

(accumulate * 1 (list 1 2 3 4 5 6 7 8 9 10))

(accumulate * 1 (map square (filter even? (list 1 2 3 4 5 6 7 8))))

(sum-odd-squares (list (list 1 2 (list 3 4)) (list 5 (list 6 7) (list 8))))

(displayln "testing map ex 2.33")
(map2 square (list 1 2 3 4 5 6))
(map2 fib (list 1 2 3 4 5))

(displayln "testing append from 2.33")
(append (list 1 2 3 4 5) (list 6 7 8 9))
(append (list 1 2 3) (list 1 (list 2 3)))

(displayln "testing length from 2.33")
(length (list 1 2 3 4 5))
(length '())
(length (list 1 2 3 4 5 6 7 8 9 10))

(displayln "testing ex 2.34")
(horner-eval 2 (list 1 3 0 5 0 1))
(horner-eval 3 (list -1 1 3 2))

(displayln "testing ex 2.35")
(count-leaves (list 1 2 (list 3 4) (list 5 (list 6 7))))
(count-leaves (list 1 2 3 4 (list 5 6)))
(displayln "testing 2.35 without enumerate-tree")
(count-leaves-r (list 1 2 (list 3 4) (list 5 (list 6 7))))
(count-leaves-r (list 1 2 3 4 (list 5 6)))

(displayln "testing 2.36")
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define s2 (list (list 1 2) (list 3 4) (list 5 6)))
(accumulate-n + 0 s)
(accumulate-n * 1 s2)

(displayln "testing 2.37")
(dot-product (list 1 2 3) (list 4 5 6))
(matrix-*-vector (list (list 1 4 7) (list 2 5 8) (list 3 6 9))
                 (list 1 2 3))
(transpose (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9)))
(matrix-*-matrix (list (list 1 2 3)
                       (list 4 5 6)
                       (list 7 8 9))
                 (list (list 1 2 3)
                       (list 3 4 5)
                       (list 5 6 7)))