#lang scheme

(require "TDAposition.rkt")
(require "TDAplayer.rkt")
(require "TDAlist-players.rkt")
(require "TDApiece.rkt")

(provide empty-column)
(provide column-full?)
(provide empty-col?)
(provide count-pieces)
(provide get-position)
(provide next-positions)
(provide next-position)
(provide get-n-piece)
(provide get-n-position)
(provide insert-piece)
(provide column-check-vertical-win)




; Descripción: Función que genera una columna vacía.
; Dom: ninguno.
; Rec: columna vacía (column). 
; Tipo recursión: No aplica.

(define (empty-column)
  '())


; Descripción: Función que verifica si una columna esta vacía.
; Dom: columna (column).
; Rec: boolean (#t si la columna está vacía, #f si no). 
; Tipo recursión: No aplica.

(define(empty-col? column)
  (null? column))


; Descripción: Función que obtiene la posición en una columna.
; Dom: columns (column).
; Rec: posición (position). 
; Tipo recursión: No aplica.

(define (get-position column)
  (if (empty-col? column) '()
      (car column)
      )
  )


; Descripción: Función que obtiene las siguientes posiciones de una columna.
; Dom: columna (column).
; Rec: columna (column).
; Tipo recursión: No aplica.

(define (next-positions column)
  (cdr column))


; Descripción: Función que obtiene la siguiente posición de una columna.
; Dom: columna (column).
; Rec: posición (position).
; Tipo recursión: No aplica.

(define (next-position column)
  (get-position (next-positions column)))


; Descripción: Función que obtiene la cantidad de fichas puestas en una columna.
; Dom: columna (column).
; Rec: número de fichas en la columna (int).
; Tipo recursión: De cola.

(define (count-pieces column)
  (define (count-pieces-aux column total)
    (cond
      [(empty-col? column) total]
      [else (count-pieces-aux (next-positions column) (+ 1 total))])
    )(count-pieces-aux column 0)
  )


; Descripción: Función que obtiene una posición específica de una columna dado un número.
; Dom: columna (column) X número de posición (int).
; Rec: posición (position).
; Tipo recursión: Natural.

(define (get-n-position column num-position)
  (cond
    [(> num-position (count-pieces column)) '()]
    [(empty-col? column) '()]
    [(= (get-num (get-position column)) num-position) (get-position column)]
    [else (get-n-position (next-positions column) num-position)])
  )


; Descripción: Función que obtiene una ficha específica de una columna dado un número.
; Dom: columna (column) X número de posición (int).
; Rec: ficha (piece).
; Tipo recursión: Natural.

(define (get-n-piece column num-piece)
  (cond
    [(> num-piece (count-pieces column)) '()]
    [(empty-col? column) '()]
    [(= (get-num (get-position column)) num-piece) (get-piece (get-position column))]
    [else (get-n-piece (next-positions column) num-piece)]))


; Descripción: Función que verifica si una columna está llena
; (se considera una columna llena cuando tiene 6 fichas jugadas).
; Dom: columna (column).
; Rec: boolean (#t si la columna está llena, #f si no). 
; Tipo recursión: No aplica.

(define (column-full? column)
  (if (eq? (count-pieces column) 6) #t
      #f
  )
)


; Descripción: Función que inserta una ficha en una columna con su respectivo número de posición.
; Dom: ficha (piece) X columna (column).
; Rec: column (column).
; Tipo recursión: No aplica.

(define(insert-piece piece column)
  (cond
    [(column-full? column) column]
    [else (cons (position (+ (count-pieces column) 1) piece) column)])
  )


; Descripción: Función que verifica si hay 4 fichas del mismo color en una columna
; y retorna el id del jugador correspondiente.
; Dom: columna (column).
; Rec: int (ID del ganador, 0 si es que no hay ganador).
; Tipo recursión: De cola.

(define (column-check-vertical-win column)
  (cond
    [(< (count-pieces column) 4) 0]
    [else (define (column-check-vertical-win-aux column count piece)
       (cond
         [(= 4 count) (get-id-piece piece)]
         [(empty-col? (next-positions column)) 0]
         [(string=? (get-color-piece (get-piece (next-position column))) (get-color-piece piece)) (column-check-vertical-win-aux (next-positions column) (+ 1 count) piece)]
         [else (column-check-vertical-win-aux (next-positions column) 1 (get-piece (next-position column)))])
            )(column-check-vertical-win-aux column 1 (get-n-piece column (count-pieces column)))]
    )
  )
