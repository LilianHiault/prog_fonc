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
