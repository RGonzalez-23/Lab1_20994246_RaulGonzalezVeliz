#lang scheme

(provide list-players)
(provide get-player)
(provide get-next-player)




; Descripción: Función que construye una lista con los dos jugadores.
; Dom: jugador-1 (player) X jugador-2 (player).
; Rec: lista de jugadores (list-players). 
; Tipo recursión: No aplica.

(define (list-players player1 player2)
  (list player1 player2))


; Descripción: Función que devuelve un jugador en la lista de jugadores.
; Dom: lista de jugadores (list-players).
; Rec: jugador (player). 
; Tipo recursión: No aplica.

(define (get-player list-players)
  (car list-players))


; Descripción: Función que entrega al siguiente jugador en la lista de jugadores.
; Dom: lista de jugadores (list-players).
; Rec: jugador (player). 
; Tipo recursión: No aplica.

(define (get-next-player list-players)
  (get-player (cdr list-players)))