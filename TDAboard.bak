#lang scheme

(require "TDAplayer.rkt")





; Descripción: Función que genera una fila de conecta 4, que es un par de numero de fila y ficha.
; Dom: num-fila (int) X ficha (piece).
; Rec: fila (row). 
; Tipo recursión: No aplica.

(define (row n-row piece)
  (cons n-row piece))


; Descripción: Función que verifica si una fila esta vacía.
; Dom: fila (row).
; Rec: boolean (#t si la fila está vacía, #f si no). 
; Tipo recursión: No aplica.

(define (empty-row? row)
  (null? row))

(define (get-num row)
  (car row))

(define (get-piece row)
  (cdr row))


(define (empty-column)
  '())

(define(empty-col? column)
  (null? column))

(define (get-row column)
  (if (empty-col? column) "Error: no pieces in column"
      (car column)
      )
  )

(define (next-rows column)
  (cdr column))

(define (next-row column)
  (get-row (next-rows column)))

(define (count-pieces column)
  (define (count-pieces-aux column total)
    (cond
      [(empty-col? column) total]
      [else (count-pieces-aux (next-rows column) (+ 1 total))])
    )(count-pieces-aux column 0)
  )

(define (get-n-piece column num-piece)
  (cond
    [(empty-col? column) "Error: piece doesn't exist"]
    [(= (get-num (get-row column)) num-piece) (get-piece (get-row column))]
    [else (get-n-piece (next-rows column) (num-piece))])
  )

(define (column-full? column)
  (if (eq? (count-pieces column) 6) #t
      #f
  )
)

(define (board)
  (list  (empty-column) (empty-column) (empty-column) (empty-column) (empty-column) (empty-column) (empty-column))
  )

(define (out-of-board? tablero)
  (null? tablero))

(define (next-columns tablero)
  (cdr tablero)
  )

(define (get-column tablero)
  (car tablero)
  )

(define (end-board? tablero)
  (null? tablero))

(define (get-n-column tablero num-column)
  (cond
    [(= num-column 1) (get-column tablero)]
    [else (get-n-column (next-columns tablero) (- num-column 1))])
  )

(define(insert-piece piece column)
  (cond
    [(column-full? column) column]
    [else (cons (row (+ (count-pieces column) 1) piece) column)])
  )

(define (append-column col-1 col-2)
  (cons col-1 col-2)
  )

(define (board-can-play? tablero)
  (define (board-can-play-aux? tablero num-column)
    (cond
      [(= num-column 8) #f]
      [(null? tablero) #t]
      [(not (column-full? (get-column tablero))) #t]
      [else (board-can-play-aux? (next-columns tablero) (+ 1 num-column))])
    )(board-can-play-aux? tablero 1)
  )

(define (update-column-board tablero n-column new-column)
  (cond
    [(= n-column 1) (append-column new-column (next-columns tablero))]
    [else (append-column (get-column tablero) (update-column-board (next-columns tablero) (- n-column 1) new-column))])
  )

(define (board-set-piece tablero n-column piece)
  (update-column-board tablero n-column (insert-piece piece (get-n-column tablero n-column)))
  )



(define (column-check-vertical-win column players)
  (cond
    [(< (count-pieces column) 4) 0]
    [else (define (column-check-vertical-win-aux column count piece players)
       (cond
         [(= 4 count) (cond
                        [(string=? piece (get-color (get-player players))) (get-id (get-player players))]
                        [else (get-id (get-next-player players))])]
         [(empty-col? (next-rows column)) 0]
         [(eqv? (get-piece (next-row column)) piece) (column-check-vertical-win-aux (next-rows column) (+ 1 count) piece players)]
         [else (column-check-vertical-win-aux (next-rows column) 1 (get-piece (next-row column)))])
            )(column-check-vertical-win-aux column 1 (get-n-piece column (count-pieces column)) players)]
    )
  )

;(define (get-n-row board num-row)
; (cond
;    [(> num-row (count-pieces (get-column board))) null]
;    [else (cons())]))

(define (board-check-vertical-win tablero players)
  (cond
    [(end-board? tablero) 0]
    [(= (column-check-vertical-win (get-column tablero) players) 0) (board-check-vertical-win (next-columns tablero) players)]
    [else (column-check-vertical-win (get-column tablero) players)])
  )

;(define (board-check-horizontal-win tablero)
  ;)