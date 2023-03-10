#lang racket/gui

;; Se dă o listă cu n imagini și un număr natural k mai mic sau egal cu dimensiunea listei. Să se genereze o noua listă cu imagini astfel:
;; - se generează toate submulțimile de dimensiune k ale listei primite
;; - se sorteaza în ordine crescătoare fiecare submulțime
;; - se suprapun imaginile din fiecare submulțime astfel încât prima imagine să fie cea mai din spate, iar ultima imagine să fie imaginea
;; din față
;; - lista de imagini cerută este lista formată din imaginile suprapuse din fiecare submulțime generată
;; - pentru a genera o imagine de fundal folosiți empty-image
;;
;; Sugestii:
;; - pentru fiecare element din listă poți alege dacă îl adaugi sau nu la submulțimea curentă. Deoarece dorim să creăm toate submulțimile
;; trebuie să luăm în calcul ambele variante.
;; - în cadrul unui apel de funcție, prima dată sunt evaluați parametrii, apoi funcția propriu zisă! Astfel, rezultatul unui apel recursiv
;; poate fi folosit drept rezultat intermediar pentru un alt apel.
;;
;; Pentru verificarea rezultatelor (în caz ca aveți un rezultat greșit) listele cu imaginile ce trebuie să rezulte se gasesc în variabilele
;; BONUS-IMAGES-RESULT1/2. Rezultatul vostru îl puteți obține prin rularea apelului image-subsets din cele două apeluri check%.

(define (image-subsets images k)
  (generate-images images k '() '()))

(define (generate-images images k subset result)
  (cond [(equal? k 0) (cons (overlay-> empty-image (mergesort subset)) result)]  ; k elemente în submutime => adaugam o solutie noua
        [(null? images) result] ; lista este nula => se intoarce lista de submultimi
        [else (generate-images (cdr images) (- k 1)  ; altfel se trece la urmatorul element din lista 
                               (cons (car images) subset)  ; elementul curent poate fi adăugat sau nu la submultimea curenta
                               (generate-images (cdr images) k subset result))]))