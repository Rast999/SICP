#lang scheme
(require racket/include)
(include "lists.rkt")

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; ############# TEST #############
(define tree1 (list (list 1 2) (list 3 4 (list 5 6))))

(scale-tree tree1 10)