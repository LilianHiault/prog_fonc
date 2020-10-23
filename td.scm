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
