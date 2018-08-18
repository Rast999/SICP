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
        (else
         (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? exp)
  (and (pair? exp) (eq? '+ (car exp))))

(define (addend s)
  (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (make-sum (caddr s) (cadddr s))))

(define (product? exp)
  (and (pair? exp) (eq? '* (car exp))))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (if (eq? (cdddr p) '())
      (caddr p)
      (append '(*) (cddr p))))

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

(deriv '(+ x 3) 'x)
(deriv '(+ (** x 3) (** x 2) x) 'x)
(deriv '(+ (** x 3) (** x 2) (** x 1)) 'x)
(deriv '(+ (** x 8) 3) 'x)
(deriv '(* 3 (** x 3) y) 'x)
(deriv '(** x 2) 'x)
(deriv '(* (* 2 x) (* 3 y) (* 1 z)) 'x)
(deriv '(+ (** x 3) (** x 2) x 1) 'x)