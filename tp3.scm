(define Prodscal
  (lambda (V W)
    (if (null? V)
	0
	(+ (* (car V)
	      (car W))
	   (Prodscal (cdr V)
		     (cdr W)
		     )))
    ))
(Prodscal '(1 2 3) '(10 20 30))

(define  (Prodscal V W)
  (sommel (map * (zip V W))))

(define zip
  (lambda (L1 L2)
    (if (or (null? L1) (null? L2))
	()
	(cons (list (car L1) (car L2))
	      (zip (cdr L1) (cdr L2)))
	)
    ))

(map (lambda (x)
       (* (car x) (cdr x)))
     (zip '(1 2 3) '(10 20 30)))

(define sommel
  (lambda (L)
    (if (null? L)
	0
	(+ (car L) (sommel (cdr L)))
	)
    ))
(sommel '(1 2 3 4))
	   

(define proscal
  (lambda (V W)
    (apply + (map * V W))))

(proscal '(1 2 3) '(10 20 30))
