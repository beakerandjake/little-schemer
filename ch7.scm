#lang sicp

; CH 7 - Friends and Relations

; is the atom included in the lat?
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
        (member? a (cdr lat)))))))

; removes each occurrence of a from the lat 
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) a)
           (multirember a (cdr lat)))
          (else (cons (car lat)
                  (multirember a
                    (cdr lat)))))))))

; is every element in the set unique?
(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))


; removes duplicate elements from the lat
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat)
              (makeset
                (multirember (car lat) 
                  (cdr lat))))))))

; is every element from set1 present in set2?
(define subset? 
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
        (and (member? (car set1) set2)
          (subset? (cdr set1) set2))))))

; are the two sets equal?  
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
      (subset? set2 set1))))

; is at least one atom in set1 present in set2?
(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
              (intersect?
                (cdr set1) set2))))))

; returns the elements in set1 present in set2
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) 
       (cons (car set1) 
         (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

; returns the elements present in both or either sets.
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
              (union (cdr set1) set2))))))

; returns the elements present in set1 but not set2
(define difference 
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) 
              (difference (cdr set1) set2))))))

; returns the elements present in all sets
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
       (else (intersect (car l-set)
               (intersectall (cdr l-set)))))))
