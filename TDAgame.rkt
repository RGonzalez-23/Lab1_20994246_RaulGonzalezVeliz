#lang scheme

(require "TDAplayer.rkt")
(require "TDAlist-players.rkt")
(require "TDAboard.rkt")
(require "TDApiece.rkt")




; Descripción: Función que contruye un TDA game.
; Dom: jugador (player) X jugador (player) X tablero (board) X turno en curso (int).
; Rec: juego (game).
; Tipo recursión: No aplica.

(define (game player-1 player-2 board current-turn)
  (list (list-players player-1 player-2) board current-turn))


; Descripción: Función que obtiene la lista de jugadores de un juego.
; Dom: juego (game).
; Rec: lista de jugadores (list-players).
; Tipo recursión: No aplica.

(define (get-list-players game)
  (car game))


; Descripción: Función que obtiene el primer jugador de un juego.
; Dom: juego (game).
; Rec: jugador (player).
; Tipo recursión: No aplica.

(define (get-first-player game)
  (get-player (get-list-players game))
  )


; Descripción: Función que obtiene el segundo jugador de un juego.
; Dom: juego (game).
; Rec: jugador (player).
; Tipo recursión: No aplica.

(define (get-second-player game)
  (get-next-player (get-list-players game))
  )


; Descripción: Función que obtiene el tablero actualizado de un juego.
; Dom: juego (game).
; Rec: tablero (board).
; Tipo recursión: No aplica.

(define (get-board game)
  (car (cdr game)))


; Descripción: Función que obtiene el turno en curso de un juego.
; Dom: juego (game).
; Rec: turno en curso (int).
; Tipo recursión: No aplica.

(define (get-current-turn game)
  (car (cdr (cdr game)))
  )


; Descripción: Función que verifica si un juego está empatado.
; Dom: juego (game).
; Rec: boolean (# t si el juego está empatado, #f si no).
; Tipo recursión: No aplica.

(define (game-is-draw? game)
  (cond
    [(not (board-can-play? (get-board game))) (cond  
                                                [(= 0 (board-who-is-winner (get-board game))) #t]
                                                [else #f])]
    [(and (= 0 (get-remaining (get-first-player game))) (= 0 (get-remaining (get-second-player game)))) #t]
    [else #f])
  )


; Descripción: Función que obtiene el primer jugador de un juego.
; Dom: juego (game).
; Rec: juagador (player).
; Tipo recursión: No aplica.

(define (game-set-end game)
  (cond
    [(game-is-draw? game) (game (player-update-stats (get-first-player game) "draw") (player-update-stats (get-second-player game) "draw") (board) 1)]
    [(not (= 0 (board-who-is-winner (get-board game)))) (cond
                                                          [(= (board-who-is-winner (get-board game)) (get-id (get-first-player game))) ((game (player-update-stats (get-first-player game) "win") (player-update-stats (get-second-player game) "loss") (board) 1))]
                                                          [else (game (player-update-stats (get-first-player game) "loss") (player-update-stats (get-second-player game) "win") (board) 1)])]      
    [else game]))

(define (game-player-set-move game player num-column)
  ())




