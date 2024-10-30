#lang scheme

(require "TDAplayer_209942461_Gonzalez.rkt")
(require "TDAboard_209942461_Gonzalez.rkt")
(require "TDApiece_209942461_Gonzalez.rkt")

(provide game)
(provide game-is-draw?)
(provide game-get-current-player)
(provide game-get-board)
(provide game-set-end)
(provide game-player-set-move)




; Descripción: Función que contruye un TDA game.
; Dom: jugador (player) X jugador (player) X tablero (board) X turno en curso (int).
; Rec: juego (game).
; Tipo recursión: No aplica.

(define (game player-1 player-2 board current-turn)
  (list player-1 player-2 board current-turn))


; Descripción: Función que obtiene el primer jugador de un juego.
; Dom: juego (game).
; Rec: jugador (player).
; Tipo recursión: No aplica.

(define (get-first-player game)
  (car game)
  )


; Descripción: Función que obtiene el segundo jugador de un juego.
; Dom: juego (game).
; Rec: jugador (player).
; Tipo recursión: No aplica.

(define (get-second-player game)
  (car (cdr game))
  )


; Descripción: Función que obtiene el tablero actualizado de un juego.
; Dom: juego (game).
; Rec: tablero (board).
; Tipo recursión: No aplica.

(define (game-get-board game)
  (car (cdr (cdr game)))
  )


; Descripción: Función que obtiene el turno en curso de un juego. El turno varía entre 1 y 2,
; estos últimos haciendo referencia al id de cada jugador (si el turno está en 1, debe jugar el que tenga id = 1).
; Por convención siempre empezará el juego el jugador cuyo id sea 1.
; Dom: juego (game).
; Rec: turno en curso (int).
; Tipo recursión: No aplica.

(define (get-current-turn game)
  (car (cdr (cdr (cdr game))))
  )


; Descripción: Función que verifica si un juego está empatado.
; Dom: juego (game).
; Rec: boolean (# t si el juego está empatado, #f si no).
; Tipo recursión: No aplica.

(define (game-is-draw? game)
  (cond
    [(not (board-can-play? (game-get-board game))) (cond  
                                                [(= 0 (board-who-is-winner (game-get-board game))) #t]
                                                [else #f])]
    [(and (= 0 (get-remaining (get-first-player game))) (= 0 (get-remaining (get-second-player game)))) #t]
    [else #f])
  )

(define (game-get-current-player game)
  (cond
    [(= (get-current-turn game) (get-id (get-first-player game))) (get-first-player game)]
    [else (get-second-player game)]))

; Descripción: Función que cambia el numero de turno.
; Dom: turno (int).
; Rec: turno (int).
; Tipo recursión: No aplica.

(define (change-turn turn)
  (cond
    [(= 1 turn) 2]
    [else 1])
  )


; Descripción: Función que devuelve un juego nuevo y con las estadídticas actualizadas de cada jugador
; en caso de que el tablero este en situación de empate o victoria de alguno de los jugadores haya ganado
; si en el tablero se puede seguri jugando retornará el mismo tablero.
; Dom: juego (game).
; Rec: juego (game).
; Tipo recursión: No aplica.

(define (game-set-end juego)
  (cond
    [(game-is-draw? juego) (game (player-update-stats (get-first-player juego) "draw")
                                 (player-update-stats (get-second-player juego) "draw") (board) 1)]
    [(not (= 0 (board-who-is-winner (game-get-board juego)))) (cond
                                                          [(= (board-who-is-winner (game-get-board juego))
                                                              (get-id (get-first-player juego)))
                                                           (game (player-update-stats (get-first-player juego) "win")
                                                                 (player-update-stats (get-second-player juego) "loss")
                                                                 (board) 1)]
                                                          [else (game (player-update-stats (get-first-player juego) "loss")
                                                                      (player-update-stats (get-second-player juego) "win")
                                                                      (board) 1)])]      
    [else juego]))


; Descripción: Función que modifica el tablero de un juego y además verifica si dicho movimiento ocasiona
; un empate o una victoria, en caso de retornará un nuevo juego con las estadísticas actualizadas.
; Dom: juego (game) X jugador (player) X numero de columna (int).
; Rec: juego (game).
; Tipo recursión: No aplica.

(define (game-player-set-move juego player num-column)
  (cond
    [(= (get-current-turn juego) (get-id player)) (cond
                                                    [(= (get-id player) (get-id (get-first-player juego)))
                                                     (game-set-end (game (rest-piece player)
                                                                         (get-second-player juego)
                                                                         (board-set-play-piece (game-get-board juego) num-column
                                                                                          (change-id-piece (piece (get-color player))
                                                                                                           (get-id player)))
                                                     (change-turn (get-current-turn juego))))]
                                                    [else (game-set-end (game (get-first-player juego) (rest-piece player)
                                                     (board-set-play-piece (game-get-board juego) num-column
                                                                      (change-id-piece (piece (get-color player)) (get-id player)))
                                                     (change-turn (get-current-turn juego))))]
                                                    )]
    [else
     (display "No corresponde turno de jugador.")
     juego]
    )
  )
