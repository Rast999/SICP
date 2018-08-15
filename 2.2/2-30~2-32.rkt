#lang scheme
(require racket/include)
(include "lists.rkt")

; ############# Helpers #############
(define (square x) (* x x))

; ############# Implementation #############
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; Ex. 2.30
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

; ############# TEST #############
(define tree1 (list (list 1 2) (list 3 4 (list 5 6))))
(define nul-tree '())

(scale-tree tree1 10)
(scale-tree nul-tree 20)
(square-tree tree1)
(square-tree nul-tree)