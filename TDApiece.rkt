#lang scheme

(define (piece color)
  (list (string-downcase color))
  )

(define (vacio? piece)
  (if (null? piece) #t
      #f)
  )