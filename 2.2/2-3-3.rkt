#lang racket

(define (element-of-set x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))



(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

; Ex. 2.60

(define (adjoin-set-a x set)
  (cons x set))

(define (remove-element-set x set)
  (define (remove-iter acc rest)
    (cond ((null? rest) acc)
          ((equal? (car rest) x) (append acc (cdr rest)))
          (else (remove-iter (cons (car rest) acc) (cdr rest)))))
  (remove-iter '() set))

(define (intersection-set-a set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set (car set1) set2)
         (cons (car set1)
               (intersection-set-a (cdr set1) (remove-element-set (car set1) set2))))
        (else (intersection-set-a (cdr set1) set2))))

(define (union-set-a set1 set2)
  (append set1 set2))

;############### TESTING #############
(element-of-set 3 '(1 2 3 4 3 2 1))
(intersection-set '(1 2 3 3 2) '(3 4 5 3 5 2))
(union-set '(1 2 3) '(3 4 5))
(union-set '(1 2 3) '(1 2 3))
(union-set '() '())
(union-set '() '(1 2 3))
(union-set '(1 2 3) '())
(union-set '(1 2 3) '(6))

(displayln "tesing ex. 2.60")
(intersection-set-a '(1 2 3 3 2) '(3 4 5 3 5 2))
(union-set-a '(1 1 2 3 2 1 3) '(3 3 4 5 5 4 3))
(remove-element-set 4 '(1 2 3 4 5))