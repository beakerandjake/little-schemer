#lang scheme
(provide atom?)


; returns true if x is an atom
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
