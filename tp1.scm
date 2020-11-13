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
	    (append (miroir (cdr L)) (list (miroir (car L))))
	    (append (miroir (cdr L)) (list (car L)))
	    ))))
(trace miroir)
(miroir '(1 (2 (3 4)) 5))

;; Renverse la liste L avec une focntion recursice terminale
(define inverseRT
  (lambda (L)
    (inverseRTAux L ())
))
;; L est ce qui reste à inverser
;; LR est la partie qui a déjà été inversé

;; (inverseRTAux '(3 4) '(2 1))
;; (inverseRTAux '(4) (3 2))
;; (inverseRTAux '() '(4 3 2 1))
;; (inverseRTAux '(1 2 3 4) ())
(define inverseRTAux
  (lambda (L LR)
    (if (null? L)
	LR
	(inverseRTAux (cdr L) (cons (car L) LR))
	)
    ))


;; 9. Membre
;; (membre x L) -> vrai si x est dans L
(define membre
  (lambda (x L)
    (if (null? L)
	#f
	(if (equal? x (car L))
	    #t
	    (membre x (cdr L))
	    ))
    ))
(membre 4 '(1 2 3 4 5))
(membre 0 '(1 2 3 4 5))
(membre '(4 5) '(1 2 3 (4 5)))
;; member


;; 10. Epure
;; (epure L) -> liste L sans double
;; () -> ()
;; (e) -> (e)
;; (e | L') ->
;;    si e appartient à L' alors (epure L')
;;    sinon (e | (epure L'))
(define epure
  (lambda (L)
    (if (null? L)
	()
	(let ((res (epure (cdr L))))
	  (if (member (car L) res)
	      res
	      (cons (car L) res)
	      )))
    ))
(epure '(2 3 4 1 2 6 3 6 7))

	

;; 12. (insere n x L) -> insère x en nième position dans L
;; (insere n x ()) -> (x)
;; (insere 1 x L) -> (x | L)
;; (insere n x (e|L') -> (e | (insere n-1 x L'))

;; (define insere
;;   (lambda (n x L)
;;     (if (= n 1)
;; 	(append (list x) L)
;; 	(append (list (car L)) (insere (- n 1) x (cdr L)))
;; 	)))

(define (insere n x L)
  (cond ((<= n 0) L)
	((= n 1) (cons x L))
	((null? L) ())
	(else
	 (cons (car L)
	       (insere x (- n 1) (cdr L)))
	 ))
  )
(insere 3 4 '(1 2 3 4 5))


;; 13.

;; 14. (inter L1 L2) -> intersection L1 inter L2
;; inter L1 () -> ()
;; inter () L2 -> ()
;; inter (e|L1') (e|L2') -> e | inter(L1' L2')
;; sinon inter L1' L2'
(define inter
  (lambda (L1 L2)
    (if (or (null? L1) (null? L2))
	()
	(if (equal? (car L1) (car L2))
	    (cons (car L1) (inter (cdr L1) (cdr L2)))
	    (inter (cdr L1) (cdr L2))
	    ))
    ))
(inter '(1 2 3 4) '(2 2 4 4 5))


(define fibo
  (lambda (n)
    (if (or (= n 0) (= n 1))
	1
	(+ (fibo(- n 1)) (fibo(- n 2)))
	)))
(fibo 5)

(define fibol
  (lambda (n)
    ;; Retourne la liste des termes de fiobonacci dans l'ordre
    (if (= n 0)
	'(1)
	(if (= n 1)
	    '(1 1)
	    (let ((ri (fibol (- n 1) )))
	      ;; Dans ri : (un-1 un-2 ... u1 u0)
	      (cons (+ (car ri) (cadr ri)) ri)
	      )))
    ))

(define fiboc
  (lambda (n)
    ;; retourne (un un-1)
    (if (= n 0)
	'(1 0)
	(if (= n 1)
	    '(1 1)
	    (let ((RCI (fiboc (- n 1))))
	      ;; (un-1 un-2)
	      (list (+ (car RCI) (cadr RCI))
		    (car RCI)))
	    ))
    ))

(fiboc 8)

;; Fibonacci recursive terminale
(define fiboRT
  (lambda (n)
    (fiboRTAux n 1 0)
    ))

(define fiboRTAux
  (lambda (n a b)
    (if (= n 0)
	a
	(fiboRTAux (- n 1) (+ a b) a))
    ))
(fiboRT 50000)

;; retourne la liste des applications de f aux éléments de L
;; """ ((f l1) (f l2) ... (f ln-1) (f ln)) """
;; () -> ()
;; (e | L') -> Mettre en tête d'une liste qui représenterait (mapkar f L')
(define mapkar
  (lambda (f L)
    (if (null? L)
	()
	(cons (f (car L))
	      (mapkar f (cdr L))))
    ))
(mapkar sqrt '(4 9 16))
(mapkar carre '(1 2 3 4 5))


;; Écrire une fonction permettant de trier une liste L
;; dans l'ordre croissant selon le tri par insertion ou tri du joueur de carte
;; insérer à la bonne place lle premier terme de L dans le reste trié
(define tri_ins
  (lambda (L)
    (if (null? L)
	()
	(insere_abp (car L)
		    (tri_ins (cdr L))))))

;; insère x à la bonne place dans L triée
(define insere_abp
  (lambda (x L)))

;; 1. Racines carrées des éléments d'une liste
;; 2. La liste des carrés des nombres
;; 3. Multiier chaque élément de L par 3
;; Ajouter x un argument d'un programme à chaque élément de L (addL x L)
;; 5. Retourner la liste des éléments divisés par 3

;; FAIRE à -> 14
;; membre
;; insere
;; Miroireure
;; inversert
;; fibort
;; mapkar
;; tri_ins
;; fri_sel
    
