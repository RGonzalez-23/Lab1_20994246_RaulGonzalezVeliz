#lang scheme

(define (player id name color wins losses draws remaining-pieces)  ; Definimos el TDA player usando la estructura de lista.
  (list id (string-downcase name) (string-downcase color) wins losses draws remaining-pieces) ; Nos aseguramos de que los strings sean minusculas.
 )