#lang scheme

(require "TDApiece_209942461_GonzalezVeliz.rkt")

(provide row-check-horizontal-win)
(provide get-ficha)
(provide count-fichas)
(provide empty-space?)




; Descripción: Función que obtiene una ficha de una fila.
; Dom: fila (row).
; Rec: ficha (piece). 
; Tipo recursión: No aplica.

(define (get-ficha row)
  (car row))


; Descripción: Función que obtiene las siguientes fichas de una fila.
; Dom: fila (row).
; Rec: fila (row). 
; Tipo recursión: No aplica.

(define (next-fichas row)
  (cdr row))


; Descripción: Función que verifica si hay una ficha en un espacio de la fila.
; Dom: fila (row).
; Rec: boolean (#t si el espacio está vacío, #f si no).
; Tipo recursión: No aplica.

(define (empty-space? row)
  (null? (get-ficha row))
  )


; Descripción: Función que verifica si estamos fuera de la fila.
; Dom: fila (row).
; Rec: boolean (#t si el estamos fuera de la fila, #f si no).
; Tipo recursión: No aplica.

(define (end-row? row)
  (null? row)
  )


; Descripción: Función que cuenta la cantidad de fichas en una fila.
; Dom: fila (row).
; Rec: numero de fichas en la fila (int).
; Tipo recursión: De cola.

(define (count-fichas row)
  (define (count-fichas-aux row total)
    (cond
      [(end-row? row) total]
      [(empty-space? row) (count-fichas-aux (next-fichas row) total)]
      [else (count-fichas-aux (next-fichas row) (+ 1 total))])
    )(count-fichas-aux row 0)
  )


; Descripción: Función que verifica si hay 4 fichas del mismo color en una columna
; y retorna el id del jugador correspondiente.
; Dom: fila (row).
; Rec: int (ID del ganador, 0 si es que no hay ganador).
; Tipo recursión: De cola.

(define (row-check-horizontal-win row)
  (cond
    [(< (count-fichas row) 4) 0]
    [else (define (row-check-horizontal-win-aux row count piece)
            (cond
              [(= count 4) (get-id-piece piece)]
              [(end-row? row) 0]
              [(empty-space? row) (row-check-horizontal-win-aux (next-fichas row) 0 piece)]
              [(string=? (get-color-piece piece) (get-color-piece (get-ficha row)))
               (row-check-horizontal-win-aux (next-fichas row) (+ 1 count) piece)]
              [else (row-check-horizontal-win-aux (next-fichas row) 1 (get-ficha row))])
            )(cond
               [(empty-space? row) (row-check-horizontal-win-aux (next-fichas row) 1 (piece "empty"))]
               [else (row-check-horizontal-win-aux (next-fichas row) 1 (get-ficha row))])]
    )
  )

