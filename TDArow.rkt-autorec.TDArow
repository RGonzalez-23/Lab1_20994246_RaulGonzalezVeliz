#lang scheme

(require "TDAplayer.rkt")
(require "TDAlist-players.rkt")

(provide row-check-horizontal-win)




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
      [(string? (get-ficha row)) (count-fichas-aux (next-fichas row) (+ 1 total))]
      [else (count-fichas-aux (next-fichas row) total)])
    )(count-fichas-aux row 0)
  )


; Descripción: Función que verifica si hay 4 fichas del mismo color en una columna
; y retorna el id del jugador correspondiente.
; Dom: fila (row) X lista de jugadores (list-players).
; Rec: int (ID del ganador, 0 si es que no hay ganador).
; Tipo recursión: De cola.

(define (row-check-horizontal-win row players)
  (cond
    [(< (count-fichas row) 4) 0]
    [else (define (row-check-horizonal-win-aux row players count piece)
            (cond
              [(= count 4) (cond
                             [(string=? piece (get-color (get-player players))) (get-id (get-player players))]
                             [else (get-id (get-next-player players))])]
              [(end-row? row) 0]
              [(empty-space? row) (row-check-horizonal-win-aux (next-fichas row) players 0 piece)]
              [(string=? piece (get-ficha row)) (row-check-horizonal-win-aux (next-fichas row) players (+ 1 count) piece)]
              [else (row-check-horizonal-win-aux (next-fichas row) players 1 (get-ficha row))])
            )(cond
               [(string? (get-ficha row)) (row-check-horizonal-win-aux row players 1 (get-ficha row))]
               [else (row-check-horizonal-win-aux (next-fichas row) players 1 "empty")])]
    )
  )

