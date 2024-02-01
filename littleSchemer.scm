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

; is at least one element from set1 present in set2?
(define subset? 
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
       (subset? (cdr set1) set2))
      (else #f))))
      

