#lang racket

;ex 2.,53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

(define (equal? items1 items2)
  (if (not (and (pair? items1)
                (pair? items2)))
      (eq? items1 items2)
      (and (eq? (car items1) (car items2))
           (equal? (cdr items1) (cdr items2)))))

(equal? '(i am robot) '(i robot am))
(equal? '(i am robot) '(i am robot))
(equal? '(i am robot) '(i am))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "Undefined expression -- DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? x1 x2)
  (and (variable? x1) (variable? x2) (eq? x1 x2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((sum? a2) (make-sum (make-sum a1 (addend a2)) (augend a2)))
        ((sum? a1) (foldl (lambda (x y)
                            (if (number? x)
                                (make-sum x y)
                                (append y (list x)))) (make-sum (addend a1) (augend a1)) (list a2)))
        (else
         (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((product? m2) (make-product (make-product m1 (multiplier m2)) (multiplicand m2)))
        ((product? m1) (foldl (lambda (x y)
                                (if (number? x)
                                    (make-product x y)
                                    (append y (list x)))) (make-product (multiplier m1) (multiplicand m1)) (list m2)))
        (else (list '* m1 m2))))

(define (sum? exp)
  (and (pair? exp) (eq? '+ (car exp))))

(define (addend s)
  (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? exp)
  (and (pair? exp) (eq? '* (car exp))))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (if (eq? (cdddr p) '())
      (caddr p)
      (cons '* (cddr p))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (exponentiation? exp)
  (and (pair? exp) (eq? '** (car exp))))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (expt base exp))
        (else
         (list '** base exp))))

; ########## testing ##########

(augend '(+ 1 2 3 4 5))
(make-sum '1 '2)
(make-sum '(+ x 3) '2)
(make-sum '1 '(+ x 3))
(make-sum '(+ 3 x) '(+ 4 5))
(make-sum '(+ x 3) '(+ y 5))
(make-sum '(+ y (** x 2) 4) '5)
(make-sum '(+ x y 5) '(+ z t 5))

(displayln "testing make-product")
(make-product '1 '2)
(make-product '(* 1 2) '3)
(make-product '1 '(* 2 3))
(make-product '(* 1 2) '(* 3 4))
(make-product '(* x 2) '(* y 3))
(make-product '(* 2 x 3) '(* z t 5))