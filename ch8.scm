#lang scheme
(require "ch5.scm")

; CH 8 - Lambda The Ultimate

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

; returns a function which compares an x value to the a value
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

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
  (insert-g
   (lambda (new old l)
    (cons new (cons old l)))))

; inserts the new value to the right of the first occurrence of the old value
(define insertR
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

; replaces first instance of old with new in the list.
(define subst
  (insert-g
   (lambda (new old l)
     (cons new l))))

; seq function which removes the current element from the array.
(define seqrem
  (lambda (new old l)
    l))

; removes the first occurence fo the item in the list
(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

