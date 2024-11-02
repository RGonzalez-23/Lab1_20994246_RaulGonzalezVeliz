#lang scheme

(provide piece)
(provide get-color-piece)
(provide get-id-piece)
(provide change-id-piece)
(provide empty-piece?)




; Descripción: Función constructora que genera una ficha de conecta 4 con un 0, el cual será cambiado por el ID del jugador.
; Dom: color (string).
; Rec: ficha (piece). 
; Tipo recursión: No aplica.

(define (piece color)
  (cons (string-downcase color) 0)
  )


; Descripción: Función que verifica si una ficha está vacía.
; Dom: ficha (piece).
; Rec: boolean (#t si la ficha está vacía, #f si no). 
; Tipo recursión: No aplica.

(define (empty-piece? piece)
  (null? piece)
  )


; Descripción: Función que obtiene el color de una ficha de conecta 4.
; Dom: ficha (piece).
; Rec: color (string). 
; Tipo recursión: No aplica.

(define (get-color-piece piece)
  (car piece)
  )


; Descripción: Función que obtiene el ID asociado a una ficha de conecta 4.
; Dom: ficha (piece).
; Rec: id de jugador (int). 
; Tipo recursión: No aplica.

(define (get-id-piece piece)
  (cdr piece)
  )


; Descripción: Función que actualiza el ID de una ficha de conecta 4.
; Dom: ficha (piece).
; Rec: ficha (piece). 
; Tipo recursión: No aplica.

(define (change-id-piece piece new-id)
  (cons (get-color-piece piece) new-id)
  )