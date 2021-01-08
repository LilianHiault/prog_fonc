;; * * * TP 5 de programmation fonctionnelle * * *
;; G2 ZZ1
;; Lilian HIAULT

;; EXERCICE 1

(define existe?
  (lambda (L P)
    (if (null? L)
	#f
	(or (P (car L))
	    (existe? (cdr L) P))
	)
    ))

;; 1.1.a
;; tous-egaux
;; () => #t
;; (a | L') => (= a (car L') ET (tous-egaux L'))

(define tous_egaux_uni
  (lambda (L)
    (if (or (null? L) (null? (cdr L)))
	#t
	(and (equal? (car L) (car (cdr L)))
	     (tous_egaux_uni (cdr L)))
	)))

(tous_egaux_uni '(1))
(tous_egaux_uni '(1 2 3))
(tous_egaux_uni '(2 2 2 2 2))
(tous_egaux_uni '(a a a))
(trace tous_egaux_uni)

;; 1.1.b
;; tous_egaux_exi
;; Si 1 non égal alors #f
(define tous_egaux_exi
  (lambda (L)
    (if (or (null? L) (null? (cdr L)))
	#t
	(if (not (equal? (car L) (car (cdr L))))
	    #f
	    (tous_egaux_exi (cdr L))
	    ))
	))

(tous_egaux_exi '())
(tous_egaux_exi '(1))
(tous_egaux_exi '(1 2 3))
(tous_egaux_exi '(2 2 2 2 2))
(tous_egaux_exi '(a a a))
(trace tous_egaux_exi)

;; 1.2
;; (tous_diff)
;; Il n'existe pas dans (cdr L) d'objet égal à (car L)
;; et (tous_diff (cdr L))

(define tous_diff
  (lambda (L)
    (cond ((null? L) #t)
	  ((null? (cdr L)) #t)
	  (else (and
		 (not (member (car L) (cdr L)))
		 (tous_diff (cdr L))))
    ) ))

(tous_diff '(1 2 3))
(tous_diff '(1 2 3 2))

(trace tous_diff)


;; EXERCICE 2

;; 2.1
;; n=0 => B
;; n=1 => f(B)
;; n=2 => f(f(B))
;; n   => f(nieme(f B (n-1)))

(define nieme
  (lambda (f B n n0)
    (if (= n n0)
	B
	(f (nieme f B (- n 1) n0) n)
	)
    ))

(trace nieme)
(nieme (lambda (x n) (+ x n)) 1 10 0)

(define somme_entier
  (lambda (n)
    (nieme + 0 n 0)))

(somme_entier 10)

(nieme (lambda (un-1 n)
	 (+ un-1 (* n n)))
       0 10 0)

(define fact
  (lambda (n)
    (nieme * 1 n 0)))
(fact 5)

(define fibo
  (lambda (m)
    (nieme (lambda (Xn-1 n)
	     (list (+ (car Xn-1)
		      (cadr Xn-1))
		   (car Xn-1)))
	   '(1 0)
	   m
	   0)))
(fibo 5)

;; EXERCICE 3

;; 3.1
;; R est reflexive si pour tt x dans E xRx

(define qqs
  (lambda (L P)
    (if (null? L)
	#t
	(and (P (car L))
	     (qqs (cdr L))))))

(define reflexive
  (lambda (R E)
    (qqs E (lambda (x)
	     (R x x)))
    ))

(define symetrique
  (lambda (R E)
    (qqs E (lambda (x)
	     (qqs E (lambda (y)
		      (or (not (R x y))
			  (R y x))))))))

;; EXERCICE 6

;; 6.1
;; L = ( (1 2 3)
;;       (4 5 6)
;;       (7 8 9) )

;; traceMat
;; L => 15
;; () => 0
;; ((a)) => a
;; L => (caar L) + trace(map cdr (cdr L))

(define traceMat
  (lambda (L)
    (if (null? L)
	0
	(+ (caar L) (traceMat (map cdr (cdr L))))
	)
    ))

(traceMat '((1 2 3)
	    (4 5 6)
	    (7 8 9)) )

;; 6.2

(define transp
  (lambda (M)
    (if (or (null? M)
	    (null? (car M)))
	()
	(cons (map car M)
	      (transp (map cdr M)))
	) ))

(transp '((1 2 3)
	  (4 5 6)
	  (7 8 9)) )

;; 6.3
;; MV = mat M * vect V

(define PS
  (lambda (V W)
    ;; Produit scalaire entre V et W
    (apply + (map * V W))
    ))

(define MV
  (lambda (M V)
    (map (lambda (L)
	   (PS L V))
	 M)
    ))

(MV '((1 2 3)
      (4 5 6)
      (7 8 9)) '(1 1 0) )

;; 6.4
;; AL : Application linéaire

(define AL
  (lambda (M)
    (lambda (V)
      (MV M V)
      ) ))


;; EXERCICE 9

;; Partition
;; () => ()
;; (1) => (1)
;; (1 2) => ((1) (2))
;; (1 2 3) => ( ((1) (2 3)) ((2) (1 3)) ((3) (1 2))

(define partition
  (lambda (E)
    (if (or (null? E) (null? (cdr E)))
	E
	(list (car E) (cdr E))
	)
    ))

(partition '(1 2 3))
(trace partition)
