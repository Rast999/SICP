#lang racket
(require racket/include)
(require math/number-theory)
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

(define (flatmap proc sequence)
  (accumulate append nil (map proc sequence)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (last seq)
  (list-ref seq (- (length seq) 1)))
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

; Ex. 2.38
(define (fold-right op init sequence)
  (accumulate op init sequence))

(define (fold-left op init sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init sequence))

; Ex. 2.39 (reverse)
(define (reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

; Example from page 128
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; Example page 129
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


; Ex. 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; Ex. 2.41

(define (sum-triplets n s)
  (filter (lambda (triplet)
            (= (accumulate + 0 triplet)
               s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 3 n))))

; Ex. 2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap
                 (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          (adjoin-position new-row k rest-of-queens))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

; append the queen to the previous ones
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

; is the last positioned queen safe on it's place?
(define (safe2? k positions)
  (let ((current (last positions)))
    (define (iter item rest count)
      (cond ((= count k) #t)
            ((or (= current item)
                 (= (abs (- current item)) (- k count)))
             #f)
            (else (iter (car rest) (cdr rest) (+ count 1)))))
    (iter (car positions) (cdr positions) 1)))

(define (safe? k positions)
  (let ((lastrow (last positions))
        (lastcol k))
    (null? (filter (lambda (row-col)
                     (and (not (= lastcol (get-col row-col)))
                          (or (= lastrow (get-row row-col))
                              (= (/ (abs (- lastrow (get-row row-col)))
                                    (abs (- lastcol (get-col row-col)))) 1))))
                   (map make-row-col
                        positions (enumerate-interval 1 k))))))

(define (make-row-col row col)
  (list row col))

(define (get-row row-col)
  (car row-col))

(define (get-col row-col)
  (cadr row-col))

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

(displayln "Testing 2.38")
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
(displayln "To give the same results for accumulate and fold-left, the op should be commutative")
(fold-right + 0 (list 1 2 3))
(fold-left + 0 (list 1 2 3))
(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))

(displayln "Testing reverse function from 2.39")
(reverse-r (list 1 2 3))
(reverse-l (list 1 2 3))

(displayln "testing  2.40")
(prime-sum-pairs 6)

(displayln "Testing permutations")
(permutations (list 1 2 3))

(displayln "Testing ex. 2.41")
(sum-triplets 6 11)

(displayln "Testing the queens problem 2.42")
(queens 10)
(safe2? 8 (list 8 4 1 3 6 2 7 7))