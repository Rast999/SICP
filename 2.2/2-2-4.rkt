#lang sicp
(#%require sicp-pict)


;(define dog (load-painter "dog.jpg"))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((small (right-split painter (- n 1))))
        (beside painter (below small small)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((small (up-split painter (- n 1))))
        (below painter
               (beside small small)))))

(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((small ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 small small))))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vector-sub (m corner1) new-origin)
                     (vector-sub (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2
                                          split-point
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame))))) 

(define (below2 painter1 painter2)
  (let ((p1 (rotate270 painter1))
        (p2  (rotate270 painter2)))
    (let ((result (transform-painter (beside p1 p2)
                                     (make-vect 1.0 0.0)
                                     (make-vect 1.0 1.0)
                                     (make-vect 0.0 0.0))))
      (lambda (frame)
        (result frame)))))

(define (square-limit3 painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (rotate270 (corner-split (rotate90 painter) n)))))

; Ex. 2.52

(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner)))))


; ########## TESTING ###########

(define v1 (make-vector 1 2))
(define v2 (make-vector 3 4))


(define fr (make-frame (make-vector 0 0)
                       (make-vector 0 5)
                       (make-vector 5 0)))




(define sg1 (make-segment (make-vector 1 2)
                          (make-vector 3 4)))

;a
(paint (segments->painter (list (make-segment (make-vect 0 0)
                                              (make-vect 1 0))
                                (make-segment (make-vect 0.99 0)
                                              (make-vect 0.99 0.99))
                                (make-segment (make-vect 0.99 0.99)
                                              (make-vect 0 0.99))
                                (make-segment (make-vect 0 0.99)
                                              (make-vect 0 0)))))

; b
(paint (segments->painter (list (make-segment (make-vect 0 0)
                                              (make-vect 1 1))
                                (make-segment (make-vect 1 0)
                                              (make-vect 0 1)))))

;c
(paint (segments->painter (list (make-segment (make-vect 0 0.5)
                                              (make-vect 0.5 0))
                                (make-segment (make-vect 0.5 0)
                                              (make-vect 1 0.5))
                                (make-segment (make-vect 1 0.5)
                                              (make-vect 0.5 1))
                                (make-segment (make-vect 0.5 1)
                                              (make-vect 0 0.5)))))


(paint (beside einstein einstein))
(paint (below einstein (number->painter 0)))
(paint (below2 einstein (number->painter 0)))

(paint (rotate270 (corner-split (rotate90 einstein) 4)))
(paint (square-limit3 einstein 4))
