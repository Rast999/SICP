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

(define (make-sum2 a1 a2)
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

; Alternative (only last line differ)
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (list a1 '+ a2))))

(define (make-product2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((product? m2) (make-product (make-product m1 (multiplier m2)) (multiplicand m2)))
        ((product? m1) (foldl (lambda (x y)
                                (if (number? x)
                                    (make-product x y)
                                    (append y (list x)))) (make-product (multiplier m1) (multiplicand m1)) (list m2)))
        (else (list '* m1 m2))))

; Alternative
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))


; Alternative for ex. 2.58

(define (sum? exp)
  (and (pair? exp) (eq? '+ (smallest-op exp))))

(define (smallest-op exp)
  (foldl (lambda (a b)
           (if (operator? a)
               (min-precedence a b)
               b))
         'maxop
         exp))

(define *precedence-table*
  '((maxop 1000)
    (minop -1000)
    (+ 0)
    (* 1)))

(define (operator? x)
  (define (loop op-pair)
    (cond ((null? op-pair) #f)
          ((eq? x (caar op-pair)) #t)
          (else (loop (cdr op-pair)))))
    (loop *precedence-table*))

(define (min-precedence a b)
  (if (precedence<? a b)
      a
      b))

(define (precedence<? a b)
  (< (precedence a) (precedence b)))

(define (precedence op)
  (define (loop op-pair)
    (cond ((null? op-pair) (error "Operator no defined -- PRECEDENCE" op))
          ((eq? op (caar op-pair)) (cadar op-pair))
          (else (loop (cdr op-pair)))))
    (loop *precedence-table*))

(define (prefix sym lst)
  (if (or (null? list) (eq? sym (car lst)))
      '()
      (cons (car lst) (prefix sym (cdr lst)))))

(define (singleton? l)
  (= (length l) 1))

(define (addend s)
  (let ((a (prefix '+ s)))
    (if (singleton? a)
        (car a)
        a)))

(define (augend s)
  (let ((a (cdr (memq '+ s))))
    (if (singleton? a)
        (car a)
        a)))


; Alternative for ex. 2.58
(define (product? exp)
  (and (pair? exp) (eq? '* (smallest-op exp))))

(define (multiplier p)
  (let ((m (prefix '* p)))
    (if (singleton? m)
        (car m)
        m)))

(define (multiplicand p)
  (let ((m (cdr (memq '* p))))
    (if (singleton? m)
        (car m)
        m)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (exponentiation2? exp)
  (and (pair? exp) (eq? '** (car exp))))

(define (base2 exp)
  (cadr exp))

(define (exponent2 exp)
  (caddr exp))

(define (make-exponentiation2 base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (expt base exp))
        (else
         (list '** base exp))))

; Alternative for ex.2.58
(define (exponentiation? exp)
  (and (pair? exp) (eq? '** (cadr exp))))

(define (base exp)
  (car exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (expt base exp))
        (else
         (list base '** exp))))

; ########## testing ##########
(displayln "testing 2.58")
(augend '(1 + 2 + 3 + 4))
(sum? (augend '(1 + x)))
(sum? '(1 + x))
(addend '(1 + x))
(augend '(1 + x + y))
(make-sum '(1 + x) '(1 + y))
(deriv '(x + (x ** 2) + z + 2) 'x)

(displayln "testing make-product")
(make-product '1 '2)
(make-product '(1 * 2) '3)
(make-product '1 '(* 2 3))
(make-product '(* 1 2) '(* 3 4))
(make-product '(* x 2) '(* y 3))
(make-product '(* 2 x 3) '(* z t 5))

(displayln "testing deriv")
(deriv '(x ** 2) 'x)
(deriv '((x ** 3) + (x ** 2) + (x * 3) + 3) 'x)
(newline)
(deriv '((x * 3) * (y * 2) * (z * 5)) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x * y * (x + 3)) 'x)