#lang scheme

(define (empty-column)
  '())

(define(empty-col? column)
  (null? column))

(define (get-piece column)
  (if (empty-col? column) "Error: no pieces in column"
      (car column)
      )
  )

(define (next-pieces column)
  (cdr column))

(define (next-piece column)
  (get-piece (next-pieces column)))

(define (count-pieces column)
  (define (count-pieces-aux column total)
    (cond
      [(empty-col? column) total]
      [else (count-pieces-aux (next-pieces column) (+ 1 total))])
    )(count-pieces-aux column 0)
  )

(define (get-n-piece column num-piece)
  (cond
    [(empty-col? column) "Error: piece doesn't exist"]
    [(= 1 num-piece) (get-piece column)]
    [else (get-n-piece (next-pieces column) (- num-piece 1))])
  )

(define (column-full? column)
  (if (eq? (count-pieces column) 6) #t
      #f
  )
)

(define (board)
  (list  (empty-column) (empty-column) (empty-column) (empty-column) (empty-column) (empty-column) (empty-column))
  )

(define (next-column tablero)
  (cdr tablero)
  )

(define (get-column tablero)
  (car tablero)
  )

(define (get-n-column tablero num-column)
  (cond
    [(= num-column 1) (get-column tablero)]
    [else (get-n-column (next-column tablero) (- num-column 1))])
  )

(define(insert-piece piece column)
  (cond
    [(column-full? column) column]
    [else (cons piece column)])
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
      [else (board-can-play-aux? (next-column tablero) (+ 1 num-column))])
    )(board-can-play-aux? tablero 1)
  )

(define (update-column-board tablero n-column new-column)
  (cond
    [(= n-column 1) (append-column new-column (next-column tablero))]
    [else (append-column (get-column tablero) (update-column-board (next-column tablero) (- n-column 1) new-column))])
  )

(define (board-set-piece tablero column piece)
  (update-column-board tablero column (insert-piece piece (get-n-column tablero column)))
  )



(define (column-check-vertical-win column)
  (cond
    [(< (count-pieces column) 4) 0]
    [else (define (column-check-vertical-win-aux column count piece)
       (cond
         [(= 4 count) piece]
         [(empty-col? (next-pieces column)) 0]
         [(eqv? (next-piece column) piece) (column-check-vertical-win-aux (next-pieces column) (+ 1 count) piece)]
         [else (column-check-vertical-win-aux (next-pieces column) 1 (next-piece column))])
            )(column-check-vertical-win-aux column 1 (get-n-piece column 1))]
    )
  )



;(define (board-check-vertical-win tablero)
;  )