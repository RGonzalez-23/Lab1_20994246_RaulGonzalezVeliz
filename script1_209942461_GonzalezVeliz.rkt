#lang scheme

(require "TDAplayer_209942461_GonzalezVeliz.rkt")
(require "TDApiece_209942461_GonzalezVeliz.rkt")
(require "TDAboard_209942461_GonzalezVeliz.rkt")
(require "TDAgame_209942461_GonzalezVeliz.rkt")


; Script de prueba para Conecta4

; 1. Creación de jugadores (16 fichas cada uno para un juego de mediana duración)
(define p1 (player 1 "Messi" "azulgrana" 0 0 0 16))
(define p2 (player 2 "Cristiano Ronaldo" "Blanco" 0 0 0 16))

; 2. Creación de piezas
(define barca-piece (piece "azulgrana"))
(define madrid-piece (piece "Blanco"))

; 3. Creación del tablero inicial
(define empty-board (board))

; 4. Creación de un nuevo juego
(define g0 (game p1 p2 empty-board 1))

; 5. Realizando movimientos para crear una victoria diagonal
(define g1 (game-player-set-move g0 p1 3))  ; Messi coloca en columna 3
(define g2 (game-player-set-move g1 p2 2))  ; Cristiano Ronaldo coloca en columna 1
(define g3 (game-player-set-move g2 p1 1))  ; Messi coloca en columna 1
(define g4 (game-player-set-move g3 p2 5))  ; Cristiano Ronaldo coloca en columna 5
(define g5 (game-player-set-move g4 p1 2))  ; Messi coloca en columna 2
(define g6 (game-player-set-move g5 p2 1))  ; Cristiano Ronaldo coloca en columna 1 
(define g7 (game-player-set-move g6 p1 3))  ; Messi coloca en columna 3 
(define g8 (game-player-set-move g7 p2 4))  ; Cristiano Ronaldo coloca en columna 4
(define g9 (game-player-set-move g8 p1 5))  ; Messi coloca en columna 5 
(define g10 (game-player-set-move g9 p2 7)) ; Cristiano Ronaldo coloca en columna 7 
(define g11 (game-player-set-move g10 p1 4)) ; Messi coloca en columna 4 (victoria horizontal)

; 6. Verificaciones durante el juego
(display "¿Se puede jugar en el tablero vacío? ")
(board-can-play? empty-board)

(display "¿Se puede jugar después de 11 movimientos? ")
(board-can-play? (game-get-board g11))

(display "Jugador actual después de 11 movimientos: ")
(game-get-current-player g11)

; 7. Verificación de victoria
(display "Verificación de victoria vertical: ")
(board-check-vertical-win (game-get-board g11))

(display "Verificación de victoria horizontal: ")
(board-check-horizontal-win (game-get-board g11))

(display "Verificación de victoria diagonal: ")
(board-check-diagonal-win (game-get-board g11))

(display "Verificación de ganador: ")
(board-who-is-winner (game-get-board g11))

; 8. Verificación de empate
(display "¿Es empate? ")
(game-is-draw? g11)

; 9. Finalizar el juego y actualizar estadísticas
(define ended-game (game-set-end g11))

; 10. Mostrar historial de movimientos
(display "Historial de movimientos: ")
(game-history ended-game) 

; 11. Mostrar estado final del tablero
(display "Estado final del tablero: ")
(game-get-board ended-game)
