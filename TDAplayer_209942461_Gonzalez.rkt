#lang scheme

(provide player)
(provide get-id)
(provide get-color)
(provide get-remaining)
(provide player-update-stats)
(provide rest-piece)




; Descripción: Función que crea un jugador.
; Dom: id (int) X name (string) X color (string) X wins (int) X losses (int) X draws (int) X remaining-pieces (int).
; Rec: player
; Tipo recursión: No aplica.

(define (player id name color wins losses draws remaining-pieces) 
  (list id (string-downcase name) (string-downcase color) wins losses draws remaining-pieces) 
 )


; Descripción: Función que obtiene el id de un jugador.
; Dom: jugador (player).
; Rec: id (int). 
; Tipo recursión: No aplica.

(define (get-id player)
  (car player))


; Descripción: Función que obtiene el nombre de un jugador.
; Dom: jugador (player).
; Rec: name (string). 
; Tipo recursión: No aplica.

(define (get-name player)
  (car (cdr player)))


; Descripción: Función que obtiene el color de un jugador.
; Dom: jugador (player).
; Rec: color (string). 
; Tipo recursión: No aplica.

(define (get-color player)
  (car (cdr (cdr player))))


; Descripción: Función que obtiene la cantidad de victorias de un jugador.
; Dom: jugador (player).
; Rec: wins (int). 
; Tipo recursión: No aplica.

(define(get-wins player)
  (car (cdr (cdr (cdr player)))))


; Descripción: Función que obtiene la cantidad de derrotas de un jugador.
; Dom: jugador (player).
; Rec: losses (int). 
; Tipo recursión: No aplica.

(define (get-losses player)
  (car (cdr (cdr (cdr (cdr player))))))


; Descripción: Función que obtiene la cantidad de empates de un jugador.
; Dom: jugador (player).
; Rec: draws (int). 
; Tipo recursión: No aplica.

(define (get-draws player)
  (car (cdr (cdr (cdr (cdr (cdr player)))))))


; Descripción: Función que obtiene la cantidad de fichas restantes de un jugador.
; Dom: jugador (player).
; Rec: remaining-pieces (int). 
; Tipo recursión: No aplica.

(define (get-remaining player)
  (car (cdr (cdr (cdr (cdr (cdr (cdr player))))))))


; Descripción: Función que le suma una victoria a un jugador.
; Dom: jugador (player).
; Rec: jugador (player). 
; Tipo recursión: No aplica.

(define (add-win jugador)
  (player (get-id jugador) (get-name jugador) (get-color jugador) (+ 1 (get-wins jugador))
          (get-losses jugador) (get-draws jugador) 21))


; Descripción: Función que le suma una derrota a un jugador.
; Dom: jugador (player).
; Rec: jugador (player). 
; Tipo recursión: No aplica.

(define (add-loss jugador)
  (player (get-id jugador) (get-name jugador) (get-color jugador) (get-wins jugador)
          (+ 1 (get-losses jugador)) (get-draws jugador) 21))


; Descripción: Función que le suma un empate a un jugador.
; Dom: jugador (player).
; Rec: jugador (player). 
; Tipo recursión: No aplica.

(define (add-draw jugador)
  (player (get-id jugador) (get-name jugador) (get-color jugador) (get-wins jugador)
          (get-losses jugador) (+ 1 (get-draws jugador)) 21))


; Descripción: Función que le resta una ficha a un jugador.
; Dom: jugador (player).
; Rec: jugador (player). 
; Tipo recursión: No aplica.

(define (rest-piece jugador)
  (player (get-id jugador) (get-name jugador) (get-color jugador) (get-wins jugador)
          (get-losses jugador) (get-draws jugador) (- (get-remaining jugador) 1)))


; Descripción: Función que actualiza el estado de un jugador
; después de una partida según el resultado de esta.
; Dom: jugador (player) X resultado (string).
; Rec: jugador (player). 
; Tipo recursión: No aplica.

(define (player-update-stats player result)
  (cond
    [(string=? result "win") (add-win player)]
    [(string=? result "loss") (add-loss player)]
    [else (add-draw player)])
  )
