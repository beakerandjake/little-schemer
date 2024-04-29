#lang scheme
(provide atom?)

; CH 1 - Toys

; returns true if x is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
