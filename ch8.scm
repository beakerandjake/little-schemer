; CH 8 - Lambda the Ultimate

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

; conses new onto old consed onto l
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

; conses old onto new consed onto l
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

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

; returns the function which maps to the atom
(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x 'o+) +)
      ((eq? x 'o*) *)
      ((eq? x 'o-) -)
      ((eq? x 'o^) expt)
      (else #f))))

; evaluates the expression and returns the value.
(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
       ((atom-to-function (operator nexp))
                          (value (1st-sub-exp nexp))
                          (value (2nd-sub-exp nexp)))))))

; given an expression like (op exp1 exp2) reutrns op
(define operator car)

; given an expression like (op exp1 exp2) returns exp1
(define 1st-sub-exp cadr)

; given an expression like (op exp1 exp2) returns exp2
(define 2nd-sub-exp caddr)

; removes each occurance of a in the list.
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) 
        (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

; returns a multirember function which uses the test? predicate.
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat))
          (multirember a (cdr lat)))
        (else (cons (car lat) (multirember a (cdr lat))))))))

; remove each element which satisfies the test? predicate
(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
        (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

; collects atoms which are not equal to a into newlat and atoms which are equal to a into seen
; the final value of the function is the result of (col newlat seen)
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) 
        (col '() '()))
      ((eq? (car lat) a)
        (multirember&co a 
          (cdr lat)
          (lambda (newlat seen)
            (col newlat
              (cons (car lat) seen)))))
      (else
        (multirember&co a
          (cdr lat)
          (lambda (newlat seen)
            (col (cons (car lat) newlat) seen)))))))

; returns true if y is an empty list
(define a-friend
  (lambda (x y)
    (null? y)))

; will always return false
(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
      (cons 'tuna seen))))

; returns true if seen is an empty list
(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen)))

; returns the length of newlat
(define last-friend
  (lambda (newlat seen)
    (length newlat)))

; returns the lengths of the newlat and seen list
(define count-friend
  (lambda (newlat seen)
    (list (length newlat) (length seen))))


