;; - - - Cours de programmation fonctionelle - - -
;; 16/12/2020


(define (rec f n vinit uinit)
  ;; Retourne Un le n ième terme de la suite donée par récurence sur N
  ;; U0 ; Un = (f Un-1 n)
  (if (= n vinit)
      uinit
      (f (rec f (- n 1) vinit uinit) n)
      ))


;; Retourne 0 + 1 + 2 + ... + n-1
(define somme_n_p_ent
  (lambda (n)
    (rec + (- n 1) 0)
    ))


;; Somme des carrés entiers de 1 à n
(define sc
  (lambda (n)
    (rec (lambda (funmun m)
	   (+ funmun (carre m)))
	 (+ n 1)
	 0)))
(sc 10)


;; * * *
;; Schéma de réduction récursif
;; * * *
(define SR
  (lambda (L B C)
    (if (null? L)
	B
	(C (car L) (SR (cdr L) B C))
	)))

(define sommel
  (lambda (L)
    ;; Value: SR
    (SR L 0 (lambda (tete RR)
	      (+ tete RR)))
    ))

(define sommel
  (lambda (L)
    ;; Value SR
    (SR L 0 +)
    ))
(sommel '(1 2 3 4))

(define prodl
  (lambda (L)
    (if (null? L)
	0
	(SR L 1 *)
	)))
(prodl '())


(define long
  (lambda (L)
    (SR L 0 (lambda (tete RR)
	      (+ 1 RR)))
    ))
(long '(1 3 2 5))

(define renverseSR
  (lambda (L)
    (SR L
	()
	(lambda (tete RR) (append RR (list tete)))
	)
    ))
(renverseSR '(2 3 4 5 6))


;; * * *
;; Schéma itératif
;; * * *
(define SI
  (lambda (L A C)
    (if (null? L)
	A
	(SI (cdr L) (C (car L) A) C)
	)
    ))
;; C est un fonction retournant un calcul entre la tête de liste et la vaiable d'accumulation courante.
;; Values: SI

(define renverse
  (lambda (L)
    (SI L () (lambda (tete A)
	       (cons tete A)))
    ))
(renverse '(2 3 4 5 6))
