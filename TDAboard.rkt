#lang scheme

(require "TDAplayer.rkt")
(require "TDAlist-players.rkt")
(require "TDAposition.rkt")
(require "TDAcolumn.rkt")
(require "TDArow-board.rkt")

; Descripción: Función que construye un tablero vacío de connect 4 a partir de 7 columnas vacías.
; Dom: No recibe parámetros de entrada.
; Rec: tablero (board).
; Tipo recursión: No aplica.

(define (board)
  (list  (empty-column) (empty-column) (empty-column) (empty-column) (empty-column) (empty-column) (empty-column))
  )


; Descripción: Función que verifica si seguimos dentro del tablero de connect 4 al momento de recorrerlo.
; Dom: tablero (board).
; Rec: boolean (#t si estamos fuera del tablero, #f si no).
; Tipo recursión: No aplica.

(define (out-of-board? tablero)
  (null? tablero)
  )


; Descripción: Función que obtiene las siguientes columnas de un tablero de connect 4.
; Dom: tablero (board).
; Rec: tablero (board).
; Tipo recursión: No aplica.

(define (next-columns tablero)
  (cdr tablero)
  )


; Descripción: Función que obtiene la columna de un tablero de connect 4.
; Dom: tablero (board).
; Rec: columna (column).
; Tipo recursión: No aplica.

(define (get-column tablero)
  (car tablero)
  )


; Descripción: Función que obtiene una columna específica de un tablero dado un número.
; Dom: tablero (board) X número de columna (int).
; Rec: columna (column).
; Tipo recursión: Natural.

(define (get-n-column tablero num-column)
  (cond
    [(= num-column 1) (get-column tablero)]
    [else (get-n-column (next-columns tablero) (- num-column 1))])
  )


; Descripción: Función que une 2 columnas dadas para formar un tablero en una función recursiva.
; Dom: columna (column) X columna (column).
; Rec: tablero (board).
; Tipo recursión: No aplica.

(define (append-column col-1 col-2)
  (cons col-1 col-2)
  )


; Descripción: Función que verifica si hay espacio dentro del tablero para jugar una ficha.
; Dom: tablero (board).
; Rec: boolean (#t si hay espacio en el tablero, #f si no).
; Tipo recursión: De cola.

(define (board-can-play? tablero)
  (define (board-can-play-aux? tablero num-column)
    (cond
      [(= num-column 8) #f]
      [(null? tablero) #t]
      [(not (column-full? (get-column tablero))) #t]
      [else (board-can-play-aux? (next-columns tablero) (+ 1 num-column))])
    )(board-can-play-aux? tablero 1)
  )


; Descripción: Función actualiza una columna en específico del tablero dado el número de la
; columna y la columna que reemplazará la columna indicada.
; Dom: tablero (board) X número de columna (int) X columna actualizada (column).
; Rec: tablero (board).
; Tipo recursión: Natural.

(define (update-column-board tablero n-column new-column)
  (cond
    [(= n-column 1) (append-column new-column (next-columns tablero))]
    [else (append-column (get-column tablero) (update-column-board (next-columns tablero) (- n-column 1) new-column))])
  )


; Descripción: Función que inserta una pieza en una columna del tablero dado el número de columna.
; Dom: tablero (board) X número de columna (int) X ficha (piece).
; Rec: tablero (board).
; Tipo recursión: No aplica.

(define (board-set-piece tablero n-column piece)
  (update-column-board tablero n-column (insert-piece piece (get-n-column tablero n-column)))
  )


; Descripción: Función que retorna una fila de un tablero dado un número de fila.
; Dom: tablero (board) X número de fila (int).
; Rec: fila de tablero (row).
; Tipo recursión: Natural.

(define (get-n-row tablero num-row)
 (cond
    [(out-of-board? tablero) null]
    [else (cons (get-n-piece (get-column tablero) num-row) (get-n-row (next-columns tablero) num-row))])
  )


; Descripción: Función que construye un TDA row-board
; (donde los elementos dentro de la lista tablero son las filas en vez de las columnas).
; Dom: tablero (board).
; Rec: fila de tablero (row).
; Tipo recursión: De cola.

(define (row-board board)
  (define (row-board-aux board count)
    (cond
      [(= 7 count) null]
      [else (append-row (get-n-row board count) (row-board-aux board (+ count 1)))])
    )(row-board-aux board 1)
  )


; Descripción: Función que verifica si alguien ha ganado de forma vertical.
; Dom: tablero (board) X lista de jugadores (list-players).
; Rec: int (ID del ganador, 0 si es que no hay ganador).
; Tipo recursión: Natural.

(define (board-check-vertical-win tablero players)
  (cond
    [(out-of-board? tablero) 0]
    [(= (column-check-vertical-win (get-column tablero) players) 0) (board-check-vertical-win (next-columns tablero) players)]
    [else (column-check-vertical-win (get-column tablero) players)])
  )


; Descripción: Función que verifica si alguien ha ganado de forma horizontal.
; Dom: tablero (board) X lista de jugadores (list-players).
; Rec: int (ID del ganador, 0 si es que no hay ganador).
; Tipo recursión: Natural.

(define (board-check-horizontal-win board players)
  (row-board-check-horizontal-win (row-board board) players))



(define (check-diagonal-win-down tablero column position players)
  (cond
    [(empty-position? position) 0]
    [else (define (check-diagonal-win-down-aux tablero column position piece count players)
            (cond
              [(= count  4) (cond
                              [(string=? piece (get-color (get-player players))) (get-id (get-player players))]
                              [else (get-id (get-next-player players))])]
              [(out-of-board? tablero) 0]
              [(empty-col? column) 0]
              [(empty-position? position) 0]
              [(string=? piece (get-piece position)) (cond
                                                       [(= 3 count) (cond
                                                                      [(string=? piece (get-color (get-player players))) (get-id (get-player players))]
                                                                      [else (get-id (get-next-player players))])]
                                                       [else (check-diagonal-win-down-aux (next-columns tablero) (get-column (next-columns tablero)) (get-n-position (get-column (next-columns tablero)) (- (get-num position) 1)) piece (+ 1 count) players)])]
              [else 0])
            )(check-diagonal-win-down-aux tablero column position (get-piece position) 0 players)]
    )
  )


(define (check-diagonal-win-up tablero column position players)
  (cond
    [(empty-position? position) 0]
    [else (define (check-diagonal-win-up-aux tablero column position piece count players)
            (cond
              [(= count  4) (cond
                              [(string=? piece (get-color (get-player players))) (get-id (get-player players))]
                              [else (get-id (get-next-player players))])]
              [(out-of-board? tablero) 0]
              [(empty-col? column) 0]
              [(empty-position? position) 0]
              [(string=? piece (get-piece position)) (cond
                                                       [(= 3 count) (cond
                                                                      [(string=? piece (get-color (get-player players))) (get-id (get-player players))]
                                                                      [else (get-id (get-next-player players))])]
                                                       [else (check-diagonal-win-up-aux (next-columns tablero) (get-column (next-columns tablero)) (get-n-position (get-column (next-columns tablero)) (+ (get-num position) 1)) piece (+ 1 count) players)])]
              [else 0])
            )(check-diagonal-win-up-aux tablero column position (get-piece position) 0 players)]
    )
  )



(define (board-check-diagonal-win board players)
  (define (board-check-diagonal-win-aux board players position)
    (cond
      [(out-of-board? board) 0]
      [(empty-position? position) (cond
                          [(out-of-board? (next-columns board))  0]
                          [else (board-check-diagonal-win-aux (next-columns board) players (get-position (get-column (next-columns board))))]
                          )]
      [(> (get-num position) 3) (cond
                                   [(= 0 (check-diagonal-win-down board (get-column board) position players)) (board-check-diagonal-win-aux board players (get-n-position (get-column board) (- (get-num position) 1)))]
                                   [else (check-diagonal-win-down board (get-column board) position players)])]
      [else (cond
              [(= 0 (check-diagonal-win-up board (get-column board) position players)) (board-check-diagonal-win-aux board players (get-n-position (get-column board) (- (get-num position) 1)))]
              [else (check-diagonal-win-up board (get-column board) position players)])]
   )
    )(board-check-diagonal-win-aux board players (get-position (get-column board)))
  )