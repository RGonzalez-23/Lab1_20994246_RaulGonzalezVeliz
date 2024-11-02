#lang scheme

(require "TDAplayer_20994246_GonzalezVeliz.rkt")
(require "TDApiece_20994246_GonzalezVeliz.rkt")
(require "TDAboard_20994246_GonzalezVeliz.rkt")
(require "TDAgame_20994246_GonzalezVeliz.rkt")


; Script de prueba para Conecta4

; 1. Creación de jugadores (7 fichas cada uno para un juego de corta duración)
(define p1 (player 1 "GokU" "NARANJA" 0 0 0 7))
(define p2 (player 2 "VegeTA" "azUl" 0 0 0 7))

; 2. Creación de piezas
(define barca-piece (piece "NARANJA"))
(define madrid-piece (piece "azUl"))

; 3. Creación del tablero inicial
(define empty-board (board))

; 4. Creación de un nuevo juego (En esta ocasión empezará jugando el player 2)
(define g0 (game p1 p2 empty-board 2))

; 5. Realizando movimientos para crear un empate por falta de fichas

(define g1 (game-player-set-move g0 (game-get-current-player g0) 1))  ; Vegeta coloca en columna 1
(define g2 (game-player-set-move g1 (game-get-current-player g1) 2))  ; Goku coloca en columna 2
(define g3 (game-player-set-move g2 (game-get-current-player g2) 1))  ; Vegeta coloca en columna 1
(define g4 (game-player-set-move g3 (game-get-current-player g3) 2))  ; Goku coloca en columna 2
(define g5 (game-player-set-move g4 (game-get-current-player g4) 2))  ; Vegeta coloca en columna 2
(define g6 (game-player-set-move g5 (game-get-current-player g5) 4))  ; Goku coloca en columna 1 
(define g7 (game-player-set-move g6 (game-get-current-player g6) 5))  ; Vegeta coloca en columna 2 
(define g8 (game-player-set-move g7 (game-get-current-player g7) 6))  ; Goku coloca en columna 1
(define g9 (game-player-set-move g8 (game-get-current-player g8) 2))  ; Vegeta coloca en columna 1 
(define g10 (game-player-set-move g9 (game-get-current-player g9) 7)) ; Goku coloca en columna 2 
(define g11 (game-player-set-move g10 (game-get-current-player g10) 3)) ; Vegeta coloca en columna 1
(define g12 (game-player-set-move g11 (game-get-current-player g11) 6))  ; Goku coloca en columna 1
(define g13 (game-player-set-move g12 (game-get-current-player g12) 4))  ; Vegeta coloca en columna 1 
(define g14 (game-player-set-move g13 (game-get-current-player g13) 3)) ; Goku coloca en columna 2 (no se pueden jugar más fichas)

; 6. Verificaciones durante el juego
(display "¿Se puede jugar en el tablero vacío? ")
(board-can-play? empty-board)

(display "¿Se puede jugar después de 14 movimientos? ")
(board-can-play? (game-get-board g14))

(display "Jugador actual después de 14 movimientos: ")
(game-get-current-player g14)

; 7. Verificación de victoria
(display "Verificación de victoria vertical: ")
(board-check-vertical-win (game-get-board g14))

(display "Verificación de victoria horizontal: ")
(board-check-horizontal-win (game-get-board g14))

(display "Verificación de victoria diagonal: ")
(board-check-diagonal-win (game-get-board g14))

(display "Verificación de ganador: ")
(board-who-is-winner (game-get-board g14))

; 8. Verificación de empate
(display "¿Es empate? ")
(game-is-draw? g14)

; 9. Finalizar el juego y actualizar estadísticas
(define ended-game (game-set-end g14))

; 10. Mostrar historial de movimientos
(display "Historial de movimientos: ")
(game-history ended-game) 

; 11. Mostrar estado final del tablero
(display "Estado final del tablero: ")
(game-get-board ended-game)