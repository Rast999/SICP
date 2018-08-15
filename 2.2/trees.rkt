#lang sicp

; Given 2 lists, concatenates them, creating one list
(define (append items1 items2)
  (if (null? items1)
      items2
      (cons (car items1) (append (cdr items1) items2))))

(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1)
        (else (+ (count-leaves (car items))
                 (count-leaves (cdr items))))))

; Ex. 2.27``y
(define (deep-reverse items)
  (cond ((null? items) nil)
        ((not (pair? items)) items)
        (else
         (append (deep-reverse (cdr items)) (cons (deep-reverse (car items)) nil)))))

(define (deep-reverse2 items)
  (if (or (null? items)
          (not (pair? items)))
      items
      (append (deep-reverse (cdr items))
              (list (deep-reverse (car items))))))

; Ex. 2.28
(define (fringe items)
  (define (iter things result)
    (cond ((null? things) (deep-reverse2 result))
          ((pair? (car things)) (iter (append (car things) (cdr things)) result))
          (else
           (iter (cdr things) (cons (car things) result)))))
  (iter items nil))

(define (fringe-r items)
  (define (helper things result)
    (cond ((null? things) result)
          ((not (pair? things)) (cons things result))
          (else
           (helper (car things) (helper (cdr things) result)))))
  (helper items nil))

; generates a list of random elements (for testing)
(define (generate-list n)
  (define (iter n result)
    (if (= n 0)
        result
        (iter (- n 1) (cons (random 1000000) result))))
  (iter n nil))

(define (gen-deep n)
  (define (iter n result)
    (let ((d (random 3)))
      (if (= n 0)
          result
          (if (= d 1)
              (iter (- n 1) (cons (cons (random 1000000) result) nil))
              (iter (- n 1) (cons (random 1000000) result))))))
  (iter n nil))

(define (benchmark f arg now)
  (f arg)
  (display (number->string (- (runtime) now)))
  (newline))


; Ex. 2.29
; Mobile structure (cons @left-branch (cons @right-branch '()))
(define (make-mobile left right)
  (cons left right))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cdr m))

; Branch structure
(define (make-branch length structure)
  (cons length structure))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cdr b))

(define (total-weight m)
  (define (calculate-branch b)
    (if (not (pair? (branch-structure b)))
        (branch-structure b)
        (+ (calculate-branch (left-branch (branch-structure b)))
           (calculate-branch (right-branch (branch-structure b))))))
  (if (not (pair? m))
      m
      (+ (calculate-branch (left-branch m))
         (calculate-branch (right-branch m)))))

; Ex. 2.29 (c)
; Detect if the mobile is balanced.
; If the length * weight is equal for both branches and all the subsequent branches are balanced
(define (balanced? m)
  (cond ((not (pair? m)) #t)
        (else
         (let ((left (left-branch m))
               (right (right-branch m)))
           (if (and (= (* (total-weight (branch-structure left)) (branch-length left)) (* (total-weight (branch-structure right)) (branch-length right)))
                    (balanced? (branch-structure left))
                    (balanced? (branch-structure right)))
               #t
               #f)))))



;(define mobile (make-mobile (make-branch 10 1)
;                            (make-branch 5 (make-mobile (make-branch 10 3)
;                                                        (make-branch 10 4)))))


;(cons (cons 10 (cons 1 '()))
;      (cond 5 (cons (cons 10 (cons 3 '()))
;                    (cons (cons 10 (cons 4 '())) '()))))
;                        mobile
;                          |
;                         / \__
;                        /   / \
;                       1   3   4

; using null? function, returning true if given an empty list
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

; Testing
;(define x (cons (list 1 2) (list 3 4)))
;(define x2 (list x x))

;(length x)
;(count-leaves x)
;(length x2)
;(count-leaves x2)

;(define z (list 1 3 (list 5 7) 9))
;(car (cdr (car (cdr (cdr z)))))

;(define z1 (list (list 7)))

;(car (car z1))

;(define z2 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z2))))))))))))

;(define x (list 1 2 3))
;(define y (list 4 5 6))
;(deep-reverse x)
;(deep-reverse2 x)
;(deep-reverse (list x y))
;(deep-reverse2 (list x y))
;(fringe (list x y))
;(fringe-r (list x y))
;(define mob (make-mobile 10 20))
;(left-branch mob)
;(right-branch mob)
;(define mobile (make-mobile (make-branch 10 (make-mobile (make-branch 6 5)
;                                                         (make-branch 5 (make-mobile (make-branch 5 5)
;                                                                                     (make-branch 5 5)))))
;                            (make-branch 5 (make-mobile (make-branch 10 3)
;                                                        (make-branch 10 4)))))
;(total-weight mobile)

(define mobtest (make-mobile (make-branch 2 1)
                             (make-branch 2 (make-mobile (make-branch 1 0.5)
                                                         (make-branch 1 0.5)))))
(balanced? mobtest)
(total-weight mobtest)