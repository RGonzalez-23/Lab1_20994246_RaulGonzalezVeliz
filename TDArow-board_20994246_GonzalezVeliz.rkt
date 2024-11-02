#lang scheme

(require "TDArow_20994246_GonzalezVeliz.rkt")

(provide append-row)
(provide row-board-check-horizontal-win)




; Descripción: Función que une 2 filas dadas para formar un row-board en una función recursiva.
; Dom: fila (row) X fila (row).
; Rec: tablero de filas (row-board).
; Tipo recursión: No aplica.

(define (append-row row-1 row-2)
  (cons row-1 row-2)
  )


; Descripción: Función que obtiene la fila de un row-board.
; Dom: tablero de filas (row-board).
; Rec: fila (row).
; Tipo recursión: No aplica.

(define (get-row row-board)
  (car row-board))


; Descripción: Función que obtiene las siguientes filas de un row-board.
; Dom: tablero de filas (row-board).
; Rec: tablero de filas (row-board).
; Tipo recursión: No aplica.

(define (next-rows row-board)
  (cdr row-board))


; Descripción: Función que verifica si estamos afuera de un row-board.
; Dom: tablero de filas (row-board).
; Rec: boolean (#t si estamos fuera del row-board, #f si no). 
; Tipo recursión: No aplica.

(define (end-row-board? row-board)
  (null? row-board))


; Descripción: Función que verifica si hay ganador en un row-board.
; Dom: tablero de filas (row-board).
; Rec: int (ID del ganador, 0 si es que no hay ganador).
; Tipo recursión: Natural.

(define (row-board-check-horizontal-win row-board)
 (cond
   [(end-row-board? row-board) 0]
   [(= (row-check-horizontal-win (get-row row-board)) 0)
    (row-board-check-horizontal-win (next-rows row-board))]
   [else (row-check-horizontal-win (get-row row-board))]
   )
  )