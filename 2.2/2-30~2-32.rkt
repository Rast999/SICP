#lang racket
(require racket/include)
(include "lists.rkt")

; ############# Helpers #############
(define (square x) (* x x))

(define (map f items)
  (if (null? items)
      '()
      (cons (f (car items)) (map f (cdr items)))))

; ############# Implementation #############
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; Ex. 2.30
; With map
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

; without map
(define (square-tree-w tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-w (car tree))
                    (square-tree-w (cdr tree))))))

; Ex. 2.31
(define (tree-map fact tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fact sub-tree)
             (fact sub-tree)))
       tree))

(define (square-tree-m tree)
  (tree-map square tree))

; ############# TEST #############
; Defining some test trees
(define tree1 (list (list 1 2) (list 3 4 (list 5 6))))
(define tree2 (list 1 2 3 4 5 6))
(define tree3 (list 1))
(define nul-tree '())

(display "testing scale-tree")
(newline)
(scale-tree tree1 10)
(scale-tree tree2 10)
(scale-tree tree3 10)
(scale-tree nul-tree 20)

(display "testing square-tree")
(newline)
(square-tree tree1)
(square-tree tree2)
(square-tree tree3)
(square-tree nul-tree)

(display "testing square-tree without map")
(newline)
(square-tree-w tree1)
(square-tree-w tree2)
(square-tree-w tree3)
(square-tree-w nul-tree)

(display "testing the 2.31")
(newline)
(square-tree-m tree1)
(square-tree-m tree2)
(square-tree-m tree3)
(square-tree-m nul-tree)