;; Lilian HIAULT
;; Programmation fonctionnelle : exercices de base

;; C-x C-e évalue l'expression devant le curseur
;; Nouveau buffer
;; M-x run-scheme

;; 0.

(define carre
  (lambda ( x )
    (* x x)))

(carre 5)

(define quad
  (lambda(x)
    (let ((cr (carre x)))
      (* cr cr))))

(quad 5)

;; 1.

(define pi 3.14)

(define cercle
  (lambda (r)
    (list(* 2 pi r)
	 (* pi (carre r)))))
(cercle 3)

;; 2.

(define LA
  '(1
    (2(6)(7))
    (3(8 12)(9))
    (4(10(13)(14(17)))
      (11(15 16)) (18))
    (5))
  )
(car la)
(cdr la)
(car (cdr LA)) ;; Sous arbre de racine 2
(car (cdr (cdr LA)))
(cadddr LA)

(cdadr (cadddr LA)) ;; Sous arbre dont la racine est 10

;; Ajouter une feuille à la racine 14

(car (cddadr (cadddr LA))) ;; Sous arbre de racine 14

;; 3.

(define fact
  (lambda (n)
    (if (zero? n)
	1
	(* n (fact (- n 1)))
	)))

(define fac
  (lambda (n)
    (if (integer? n)
	(fact n)
	"n n'est pas un entier"
	)))

(fac 5)

;; 4. Somme des n premiers entiers

;; 5. (long L)

(define long
  (lambda (L)
    (if (null? L)
	0
	(+ 1 (long (cdr L)))
	)))

(long '(1 2 3 5 3 1 0))

;; 6.
;; Ajouter x :
;; () -> (list x)
;; (e|L') -> (e|(ajoute x L)
(define ajouter
  (lambda (x L)
    (if (null? L)
	(list x)
	(cons (car L) (ajouter x (cdr L)))
	)))

(ajouter 1 '(2 3 4 5 6))

(trace ajouter)
(ajouter 2 '(1 2 3))
(untrace ajouter)

;; Renverse L :
;; () -> ()
;; (car|cdr) -> (renverse cdr|car)
(define renverse
  (lambda (L)
    (if (null? L)
	'()
	(append (renverse (cdr L)) (list (car L)))
	)))

(trace renverse)
(renverse '(1 (2 (3 4)) 5))

;; Miroir :
;; Ex : (1 (2 (3 4)) 5) -> (5 ((4 3) 2) 1)
;; () -> ()
;; (e|L') -> (miroir L'|miroir e) si e est une liste
;;        -> (miroir L'| e) sinon

(define miroir
  (lambda (L)
    (if (null? L)
	'()
	(if (list? (car L))
	    (append (miroir (cdr L)) (list (car L)))
	    ))))

(miroir '(1 (2 (3 4)) 5))
