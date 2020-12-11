;; 1.

;; Tri par insertion
;; (apres o1 o2) est une fonction qui retourne #t si son 1er arg o1 est apres son second o2
(define tri_ins
  (lambda (L apres)
    (if (null? L)
	()
	(inserer (car L)
		 (tri_ins (cdr L) apres)
		 apres)
	)))

(define inserer
  (lambda (e L apres)
    (if (null? L)
	(list e)
	(if (apres e (car L))
	    (cons (car L)
		  (inserer e (cdr L) apres))
	    (cons e L)))
    ))

(tri_ins '( 2 4 91 9 2 1 7 2 9 26) >)
(tri_ins '("xavier" "Laurent" "anais" "pierre" "roger" "François") string-ci>?)

(define fic
  '(("xavier" 27 150)
    ("laurent" 30 180)
    ("Pierre" 45 170)
    ("Albert" 10 130)
    ("agathe" 25 165)))
fic
;; Trier fic selon l'âge
(tri_ins fic
	 pers_app)
(tri_ins fic
	 (lambda(P1 P2)
	   (> (cadr P1) (cadr P2))))

;; Trier selon la taille
(tri_ins fic
	 (lambda(P1 P2)
	   (> (caddr P1) (caddr P2))))

;; 2.
;; Ecrire (comp f g) retourne la fonction f o g
(define comp
  (lambda (f g)
    (lambda (x)
    (f (g x)))))

(define carre
  (lambda (x)
  (* x x)))

((comp carre sqrt) 3)

;; 3.
;; Dérivé
(define derive
    (lambda (f)
      (lambda (x)
	(let ((h 0.0001))
          (/ (- (f (+ x h)) (f (- x h)) ) (* 2 h)))
      )))
((derive carre) 8)

;;  4.
(define tauxacc
  (lambda (h)
    (lambda (f)
      (lambda (x)
	(/ (- (f (+ x h)) (f (- x h)) ) (* 2 h)))
      )))
(((tauxacc 0.0001) carre) 3)


(define pi 3.1415926535)


(map (lambda (y)
       (((tauxacc 0.0001) sin) y)
       (list (/ pi 4) (/ pi 3) (/ pi 2) pi (* 3 (/ pi 4))))

       (map cos
	    (list (/ pi 4) (/ pi 3) (/ pi 2) pi (* 3 (/ pi 4)))))
	    
;; 5.
(map sqrt '(4 9 16 25 36 49))
(map carre '(2 3 4 5 6 7))

;; Retourne la liste des couple
(define zip
  (lambda (L1 L2)
    (if (or (null? L1) (null? L2))
	()
	(cons (list (car L1) (car L2))
	      (zip (cdr L1) (cdr L2))))
    ))
(map list '(1 2 3) '(un deux trois))

(map (lambda (x) (+ x 3))
     '(1 2 3 4 5))

(define (mult x L)
  (map
   (lambda (y)
     (* y x))
   L))
(mult 2 '(1 2 3 4 5))

;; 6.
;; Retourne la liste des termes pairs
(map even? '(1 2 3 4 5 6))
(define (mapkan f L)
  (if (null? L)
      ()
      (append (f (car L))
	      (mapkan (cdr L)))))
;; => append_map

(append-map (lambda (x)
       (if (even? x)
	   (list x)
	   ()
	   ))
	'(1 2 3 4 5 6))


;; (sup L x) retourne la liste des termes de L supérieurs à x

(define (sup L x)
  (append-map
   (lambda (y)
     (if (> y x)
	 (list y)
	 ()
	 ))
   L))

(sup '( 126 4  1 4 94 2 04 48 47 2) 12)


;; 7
;; ( (1 2 3)
;;   (4 5 6)
;;   (7 8 9) )

;; ((1 2 3) (4 5 6) (7 8 9)) => trace =>  1+5+9 = 15

;; () => ?
;; ((x)) => x
;; ((1 2 3) (4 5 6) (7 8 9)) => 1 + (trace ((5 6) (8 9)) )

(define traceMat
  (lambda (L)
    (if (null? L)
	0
	(+ (caar L)
	 (traceMat (map cdr (cdr L)))
	 ))))

(trace traceMat)
(traceMat '((1 2 3) (4 5 6) (7 8 9)))

;; 8
;; (P '( 2 3)) => (() (2) (3) (2 3))
;; (P '(3)) => (() (3))
;; (P '(2 3)) => (P '(3)) concat (P '(2)) concat '(2 3)
;; (P '()) => (())

;; P(1 2 3) => () + (1) + (1 2 3) + P(2 3)

(define ajouterssliste
  (lambda (L)
    (if (null? L)
	()
	(cons (cons 1 (car L))
	      (ajouterssliste (cdr L))))))
(define ajouterssliste
  (lambda (L)
    (map (lambda (x)
	   (cons 1 x))
	 L)))

(define P
  (lambda (L)
    (if (null? L)
	'(())
	(let  ((Resinf (P (cdr L))))
	  (append
	   Resinf
	   (map (lambda (x)
		  (cons (car L) x))
		Resinf))))))
(trace P)
(P '(1 2 3))


;; 9

;; 9.0 : (card E)^n
;; (PC '(1) 3) => ((1 1 1))
;; (PC '(0 1) 2) => ((0 0) (0 1) (1 0) (1 1))
;; (PC '(0 1) 1) => ((0) (1))

;; Pour chaque sous liste on l'append à chacune des sous liste de taille n-1

(define PC
  (lambda (L n)
    (if (zero? n)
	'(())
	(let ((res (PC L (- n 1)) ))
	  ;; res est une liste qui contient des listes
	  (append-map
	   (lambda (x)
	     (map
	      (lambda (y)
		(cons x y))
	      res))
	   L)))))

(PC '(0 1) 3)
