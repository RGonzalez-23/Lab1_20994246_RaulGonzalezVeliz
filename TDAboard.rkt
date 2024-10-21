#lang scheme


(define (fila n_fila piece)
  (cons n_fila piece))

(define board
  (list  (list (fila 1 "vacio") (fila 2 "vacio") (fila 3 "vacio") (fila 4 "vacio") (fila 5 "vacio") (fila 6 "vacio"))
         (list (fila 1 "vacio") (fila 2 "vacio") (fila 3 "vacio") (fila 4 "vacio") (fila 5 "vacio") (fila 6 "vacio"))
         (list (fila 1 "vacio") (fila 2 "vacio") (fila 3 "vacio") (fila 4 "vacio") (fila 5 "vacio") (fila 6 "vacio"))
         (list (fila 1 "vacio") (fila 2 "vacio") (fila 3 "vacio") (fila 4 "vacio") (fila 5 "vacio") (fila 6 "vacio"))
         (list (fila 1 "vacio") (fila 2 "vacio") (fila 3 "vacio") (fila 4 "vacio") (fila 5 "vacio") (fila 6 "vacio"))
         (list (fila 1 "vacio") (fila 2 "vacio") (fila 3 "vacio") (fila 4 "vacio") (fila 5 "vacio") (fila 6 "vacio"))
         (list (fila 1 "vacio") (fila 2 "vacio") (fila 3 "vacio") (fila 4 "vacio") (fila 5 "vacio") (fila 6 "vacio"))
         ))

(define (b0 tablero)
  board)

(define (board-can-play? tablero)
  1)