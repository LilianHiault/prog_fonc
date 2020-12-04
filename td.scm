;; Programmation fonctionnelle
;; 14/10/2020
;; Analyse de problème

(define longueur
  ;; Calcule la longueur d'une liste
  ;; L appartient à ENSL
  ;; L s'écrit () ou [e|L']
  ;; codomaine un entier positif

  (lambda(L)
    (if (null? L) ;; Vrai si la liste est vide
	0
	(+ 1 (longueur (cdr L))))))
(longueur '(1 2 3 4))
(longueur ())



(define renverse
  ;; Inverse l'ordre des éléments dans une liste 
  ;; L est une liste
  (lambda(L)
    ;; () -> () ou
    ;; L = [E|L'] -> la liste L' renversée suivie de E :
    ;; (append L' (list E))
    (if (null? L)
	()
	;; (E|L')
	(append (renverse(cdr L)) (list (car L))))))
(renverse '(1 2 3 4 5))

;; Produit Scalaire de vecteurs de même taille
(define PS1
  (lambda (V W)
    (if (null? V)
	0
	(+ (* (car V)
	      (car W))
	   (PS1 (cdr V)
		(cdr W))))
    ))
(PS1 '(1 2 3 4) '(1 2 3 4))

;; Produit sclaire
(define PS2
  (lambda (V W)
    (somme_L (ptt V W))))

;; Calcule les produits Vi * Wi
(define ptt
  (lambda (V W)
    (if (null? V)
	()
	(cons (* (car V) (car W))
	      (ptt (cdr V) (cdr W))))))
(define somme_L
  (lambda (L)
    (if (null? L)
	0
	(+ (car L)
	   (somme_L (cdr L))))))
(PS2 '(1 2 3 4) '(1 1 1 1))


(define zip
  (lambda (L1 L2)
    ;; (if (null? L1))
    ()
    (cons (list (car L1)
		(car L2)
		(zip (cdr L1)
		     (cdr L2)
		     )))))
(zip '(1 2 3 4) '(1 1 2 2))

;; Réalise les produits des termes des couples de la liste
(define prodc
  (lambda (LC)
    (if (null? LC)
	()
	(cons (* (caar LC)
		 (cadar LC))
	      (prodc (cdr LC))))))
(prodc (zip '(1 2 3 4) '(1 1 2 2)))

(define PS3
  (lambda (V W)
    (if (null? V)
	0
	(let* ((lc (zip V W))
	       (P (prodc lc)))
	  (somme_L P)))))


;; TRI par INSERTION d'une liste de nombres
(define tri_ins
  (lambda (L)
    (if (null? L)()
	(inserer (car L)
		 (tri_ins (cdr L)) )
	)))
(define inserer
  (lambda (x L)
    ;; L est triés
    (if (null? L)
	(list x)
	(if (> x (car L))
	    (cons (car L)
		  (inserer x (cdr L)))
	    (cons x L))
	)))
(tri_ins '(3 7 5 2 9 1))

;; TRIER ordre décroissant
;; => inverser le sens de la focntion de comparaison
		  
		  
(define inserer
  (lambda (x L)
    ;; L est triés
    (if (null? L)
	(list x)
	(if (< x (car L))
	    (cons (car L)
		  (inserer x (cdr L)))
	    (cons x L))
	)))

;; Tier des chaines de caractères
(tri_ins '("eric" "Thomas" "Bastien" "adrien"))

(string-ci>? "eric" "Thomas")
(string>? "Eric" "bastien")

(define inserer
  (lambda (x L)
    ;; L est triés
    (if (null? L)
	(list x)
	(if (> x (car L))
	    (cons (car L)
		  (inserer x (cdr L)))
	    (cons x L))
	)))
(define (compare_age p& p2)
  (if (> (cadr p1) (cadr p2))
      #t
      #f))


(define compare
  (lambda (p1 p2)
    ;; Retourne vrai si p1 est après p2 selon le nom
    (string>? (car p1) (car p2))))

;; (define tri_ins
;;   (lambda (L FC)
;;     (letrec
;; 	((inserer
;; 	  (lambda (x L)
;; 	    (if (null? L) (list x)
;; 		(if (FC x (car L))
;; 		    (cons (car L)
;; => voir poly
