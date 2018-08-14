#lang sicp

; accessing list's nth element (beginning with 0)
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

; using null? function, returning true if given an empty list
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

; same function as above, but iterative
(define (length-i items)
  (define (iter items count)
    (if (null? items)
        count
        (iter (cdr items) (+ count 1))))
  (iter items 0))

; Given 2 lists, concatenates them, creating one list
(define (append items1 items2)
  (if (null? items1)
      items2
      (cons (car items1) (append (cdr items1) items2))))

; Returns the last pair from the list
; Ex: list(23 72 149 34) => (34)
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

; Reversing the list
; Ex: list(1 2 3 4) => list(4 3 2 1)
(define (reverse items)
  (if (null? items)
      nil
      (append (reverse (cdr items)) (cons (car items) nil))))

; ex. 2.19

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? items)
  (null? items))

(define (except-first-denomination items)
  (cdr items))

(define (first-denomination items)
  (car items))


; Ex. 2.20

(define (same-parity x . items)
  (define (construct x items)
    (cond ((null? items) (list x))
          ((even? (+ x (car items))) (cons x (construct (car items) (cdr items))))
          (else
           (construct x (cdr items)))))
  (define (construct-i items result)
    (cond ((null? items) (reverse result))
          ((even? (+ x (car items))) (construct-i (cdr items) (cons (car items) result)))
          (else (construct-i (cdr items) result))))
  (construct-i items (list x)))

; Map function - given a 1 arument function and a list
; returns a list with each element changed by the argument function
(define (map f items)
  (if (null? items)
      nil
      (cons (f (car items)) (map f (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

; Squares the argumen
(define (square x)
  (* x x))

; Ex. 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-m items)
  (map (lambda (x) (* x x)) items))

; Ex. 2.22
(define (square-list-i items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list-i2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; Ex. 2.23
(define (for-each f items)
  (cond ((null? items) #t)
        (else (f (car items))
              (for-each f (cdr items)))))

; Testing
(define test1 (list 1 2 3 4 5 6))
(define test2 (list 7 8 9 10))

;(list-ref test1 0)
;(length test1)
;(length-i test1)
;(append test1 test2)
;(append test2 test1)
;(last-pair test1)
;(last-pair test2)
;(last-pair (append test2 test1))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
;(cc 12 us-coins)
;(same-parity 1 2 3 4 5 6 7 8 9)
;(same-parity 2 3 4 5 6 7 8 9 10 11 12)
;(same-parity 2 20 140 151 160 165 15432) 

;(map (lambda (x) (* x x)) us-coins)

;(map abs (list 10 -1 -7 8 -12 21))
;(scale-list us-coins 10)

(square-list us-coins)
(square-list-m us-coins)
(square-list (list 1 2 3 4 5 6))
(square-list-m (list 1 2 3 4 5 6))
(square-list-i (list 1 2 3 4 5 6))
(square-list-i2 (list 1 2 3 4 5 6))
(for-each (lambda (x) (newline) (display x)) us-coins)
(for-each (lambda (x) (newline) (display (* x x))) us-coins)