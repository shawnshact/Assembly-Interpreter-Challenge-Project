(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (lst) (append (list first) lst)) rests)
)

(define (zip pairs)
  (list (map car pairs) (map cadr pairs)))

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (count_up n s)
    (if (null? s) nil
      (cons (list n (car s)) (count_up (+ n 1) (cdr s)))
    )
  )
  (count_up 0 s)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((null? denoms) nil)
    ((< total 0) (list-change total (cdr denoms)))
    ((= total 0) (list nil))
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
  )
)
; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (define (helper body)
    (map
      (lambda (s)
        (cond
          ((atom? s) s)
          ((let? s) (let-to-lambda s))
          (else s)
        )) body)
  )
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (append (list 'lambda params) (helper body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
            (append (list (append (list 'lambda (car (zip values))) (helper body))) (helper (cadr (zip values))))
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (helper expr)
         ; END PROBLEM 19
         )))
;(let-to-lambda '(let ((a 1) (b 2)) (+ a b)))
(let-to-lambda '(lambda (x) a (let ((a x)) a)))
(let-to-lambda '(let ((a x)) a))
