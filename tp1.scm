;; Lilian HIAULT
;; Programmation fonctionnelle : exercices de base

;; C-x C-e évalue l'expression devant le curseur
;; Nouveau buffer
;; M-x run-scheme

;; 0

(define carre
  (lambda ( x )
    (* x x)))

(carre 5)

(define quad
  (lambda(x)
    (let ((cr (carre x)))
      (* cr cr))))

(quad 5)

;; 1

(define pi 3.14)

(define cercle
  (lambda (r)
    (list(* 2 pi r)
	 (* pi (carre r)))))
(cercle 3)

;; 2

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
