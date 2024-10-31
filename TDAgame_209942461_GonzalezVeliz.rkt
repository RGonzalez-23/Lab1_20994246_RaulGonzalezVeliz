#lang scheme

(require "TDAplayer_209942461_GonzalezVeliz.rkt")
(require "TDAboard_209942461_GonzalezVeliz.rkt")
(require "TDApiece_209942461_GonzalezVeliz.rkt")

(provide game)
(provide game-is-draw?)
(provide game-get-current-player)
(provide game-get-board)
(provide game-set-end)
(provide game-player-set-move)
(provide game-history)



; Descripción: Función que contruye un TDA game y verifica que los datos ingresados sean válidos.
; Dom: jugador (player) X jugador (player) X tablero (board) X turno en curso (int).
; Rec: juego (game).
; Tipo recursión: No aplica.

(define (game player-1 player-2 board current-turn)
  (cond
    [(string=? (get-color player-1) (get-color player-2)) (display "Juego inválido (ambos jugadores tienen el mismo color de ficha). Inténtelo de nuevo.")]
    [(or (> (get-remaining player-1) 21) (> (get-remaining player-2) 21)) (display "Juego inválido (Uno o ambos jugadores poseen una cantidad de fichas inválida). Inténtelo de nuevo.")]
    [(= (get-id player-1) (get-id player-2)) (display "Juego inválido (ambos jugadores tienen el mismo ID). Inténtelo de nuevo.")]
    [(not (or (= 1 current-turn) (= 2 current-turn))) (display "Juego inválido (el turno no es válido para ninguno de los jugadores) Inténtelo de nuevo")]
    [else (list player-1 player-2 board current-turn (list "Tablero vacío"))])
  )


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


; Descripción: Función que obtiene el historial de un juego.
; Dom: juego (game).
; Rec: History.
; Tipo recursión: No aplica.

(define (game-history game)
  (cdr (cdr (cdr (cdr game))))
  )


; Descripción: Función que construye el par columna-color para registrar en el historial de un juego.
; Dom: numero de columna (int) X  color (str).
; Rec: movimiento (movement).
; Tipo recursión: No aplica.

(define (movement column color)
  (cons column color)
  )


; Descripción: Función que actualiza un historial.
; Dom: History X movimiento (movement).
; Rec: History.
; Tipo recursión: No aplica.

(define (update-movement-history history movement)
  (cons movement history)
  )


; Descripción: Función que actualiza el historial dentro de un juego.
; Dom: juego (game) X History.
; Rec: juego (game).
; Tipo recursión: Natural.

(define (game-history-update game updated-history)
  (cond
    [(null? (cdr game)) updated-history]
    [else (cons (car game) (game-history-update (cdr game) updated-history))])
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


; Descripción: Función obtiene el jugador del turno en curso.
; Dom: juego (game).
; Rec: boolean (# t si el juego está empatado, #f si no).
; Tipo recursión: No aplica.

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
    [(game-is-draw? juego) (game-history-update (game (player-update-stats (get-first-player juego) "draw")
                                 (player-update-stats (get-second-player juego) "draw") (game-get-board juego) (change-turn (get-current-turn juego))) (update-movement-history
                                                                                                                                         (game-history juego)
                                                                                                                                         (list "Fin de juego. Generando siguiente tablero vacío...")))]
    [(not (= 0 (board-who-is-winner (game-get-board juego)))) (cond
                                                          [(= (board-who-is-winner (game-get-board juego))
                                                              (get-id (get-first-player juego)))
                                                           (game-history-update (game (player-update-stats (get-first-player juego) "win")
                                                                 (player-update-stats (get-second-player juego) "loss")
                                                                 (game-get-board juego) (change-turn (get-current-turn juego))) (update-movement-history (game-history juego)
                                                                                                                                          (list "Fin de juego. Generando siguiente tablero vacío...")))]
                                                          [else (game-history-update (game (player-update-stats (get-first-player juego) "loss")
                                                                      (player-update-stats (get-second-player juego) "win")
                                                                      (game-get-board juego) (change-turn (get-current-turn juego))) (update-movement-history (game-history juego) (list "Fin de juego. Generando siguiente tablero vacío...")))])]      
    [else juego]))


; Descripción: Función que modifica el tablero de un juego y además verifica si dicho movimiento ocasiona
; un empate o una victoria, en caso de retornará un nuevo juego con las estadísticas actualizadas.
; Dom: juego (game) X jugador (player) X numero de columna (int).
; Rec: juego (game).
; Tipo recursión: No aplica.

(define (game-player-set-move juego player num-column)
  (cond
    [(= 0 (get-remaining player)) (display "Jugador sin fichas para jugar./n") juego]
    [(= (get-current-turn juego) (get-id player)) (cond
                                                    [(= (get-id player) (get-id (get-first-player juego)))
                                                      (game-history-update (game (rest-piece player)
                                                                         (get-second-player juego)
                                                                         (board-set-play-piece (game-get-board juego) num-column
                                                                                          (change-id-piece (piece (get-color player))
                                                                                                           (get-id player)))
                                                     (change-turn (get-current-turn juego))) (update-movement-history (game-history juego)
                                                                                                                      (movement num-column (get-color player))))]
                                                    [else (game-history-update (game (get-first-player juego) (rest-piece player)
                                                     (board-set-play-piece (game-get-board juego) num-column
                                                                      (change-id-piece (piece (get-color player)) (get-id player)))
                                                     (change-turn (get-current-turn juego))) (update-movement-history (game-history juego)
                                                                                                                       (movement num-column (get-color player))))]
                                                    )]
    [else
     (display "No corresponde turno de jugador.")
     juego]
    )
  )
