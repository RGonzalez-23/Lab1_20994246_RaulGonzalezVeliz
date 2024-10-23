#lang scheme

(define (player id name color wins losses draws remaining-pieces)  ; Definimos el TDA player usando la estructura de lista.
  (list id (string-downcase name) (string-downcase color) wins losses draws remaining-pieces) ; Nos aseguramos de que los strings sean minusculas.
 )

(define (get-id player)
  (car player))

(define (get-name player)
  (car (cdr player)))

(define (get-color player)
  (car (cdr (cdr player))))

(define(get-wins player)
  (car (cdr (cdr (cdr player)))))

(define (get-losses player)
  (car (cdr (cdr (cdr (cdr player))))))

(define (get-draws player)
  (car (cdr (cdr (cdr (cdr (cdr player)))))))

(define (get-remaining player)
  (car (cdr (cdr (cdr (cdr (cdr (cdr player))))))))


(define (add-win jugador)
  (player (get-id jugador) (get-name jugador) (get-color jugador) (+ 1 (get-wins jugador)) (get-losses jugador) (get-draws jugador) (get-remaining jugador)))

(define (add-loss jugador)
  (player (get-id jugador) (get-name jugador) (get-color jugador) (get-wins jugador) (+ 1 (get-losses jugador)) (get-draws jugador) (get-remaining jugador)))

(define (add-draw jugador)
  (player (get-id jugador) (get-name jugador) (get-color jugador) (get-wins jugador) (get-losses jugador) (+ 1 (get-draws jugador)) (get-remaining jugador)))

(define (rest-piece jugador)
  (player (get-id jugador) (get-name jugador) (get-color jugador) (get-wins jugador) (get-losses jugador) (get-draws jugador) (- (get-remaining jugador) 1)))