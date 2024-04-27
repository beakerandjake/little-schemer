#lang sicp

; returns true if x is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; returns true if the two arguments are the same atom
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

; returns true if the two arguments are lists and are equal
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and (equal? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

; returns true if s1 and s2 are the same atom, or if they are the same list. 
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
       #f)
      (else (eqlist? s1 s2)))))

; returns a function which compares an x value to the a value
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; returns a rember function which compares elements using the test? function.
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l)
                    ((rember-f test?) a
                                      (cdr l))))))))

; returns an insertL function which compares elements using the test? function.
; the insertL function inserts the new value to the left of the first occurrence of the old value
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l))
         (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))

; returns an insertR function which compares elements using the test? function.
; the insertR function inserts the new value to the right of the first occurrence of the old value
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? old (car l))
         (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old (cdr l))))))))

; conses the first arg onto the result of consing the second arg onto the third arg
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

; conses the second arg onto the result of consing the first arg onto the third arg
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

; returns a functinon which uses seq to cons the new value to the first occurance of the old value in l.
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

; inserts the new value to the left of the first occurrence of the old value
(define insertL
  (insert-g seqL))

; inserts the new value to the right of the first occurrence of the old value
(define insertR
  (insert-g seqR))