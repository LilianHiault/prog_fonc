;; * * * TP 5 de programmation fonctionnelle * * *
;; G2 ZZ1
;; Lilian HIAULT

;; EXERCICE 1

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
;; Idem : faux si un seul couple (car L) = (cdr L) ou () ou (a)
;;        vrai sinon


;; EXERCICE 2

;; 2.1
;; n=0 => B
;; n=1 => f(B)
;; n=2 => f(f(B))
;; n   => f(nieme(f B (n-1)))

(define nieme
  (lambda (f B n)
    (if (= n 0)
	B
	(f (nieme f B (- n 1)))
	)
    ))

(trace nieme)
(nieme (lambda (n) (+ n 1)) 5 10)

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
