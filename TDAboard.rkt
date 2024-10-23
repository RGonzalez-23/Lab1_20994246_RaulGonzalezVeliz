#lang scheme

(define (empty-column)
  '())

(define(empty-col? column)
  (null? column))

(define (next-piece column)
  (cdr column))

(define (get-piece column)
  (car column))

(define (count-pieces column)
  (define (count-pieces-aux column total)
    (cond
      [(empty-col? column) total]
      [else (count-pieces-aux (next-piece column) (+ 1 total))])
    )(count-pieces-aux column 0)
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
    [(eq? num-column 1) (get-column tablero)]
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
      [(eq? num-column 8) #f]
      [(null? tablero) #t]
      [(not (column-full? (get-column tablero))) #t]
      [else (board-can-play-aux? (next-column tablero) (+ 1 num-column))])
    )(board-can-play-aux? tablero 1)
  )

(define (update-column-board tablero n-column new-column)
  (cond
    [(eq? n-column 1) (append-column new-column (next-column tablero))]
    [else (append-column (get-column tablero) (update-column-board (next-column tablero) (- n-column 1) new-column))])
  )

(define (board-set-piece tablero column piece)
  (update-column-board tablero column (insert-piece piece (get-n-column tablero column))))

