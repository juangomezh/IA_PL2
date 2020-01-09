#lang Racket
(require "pl2.rkt" rackunit)

(check-match (cambiarColor 'negra) 'blanc)

(check-match (cambiarFicha tab 1 'blanc) (list
   'libre 'blanc 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre 
   'libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre  
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre))

(check-eq? (voltear? tab 10 'blanc 9) #f)

(check-match (voltear tab 1 'blanc 7) empty)

(check-eq? (outOfRange 100) #t)

(check-eq? (outOfRangeFila 0) #t)

(check-eq? (outOfRangeColumna 0) #t)

(check-eq? (outOfRangeDiagonal 0) #t)

(check-eq? (flip-piece tab 1 'negra 9) #f)

(check-eq? (comprobarAlrededor tab '(11 12 13 19 20 21 27 28 29)) #t)

(check-match (cambiarFichas tab 0 'blanc) empty)

(check-eq? (movimientoLegal tab 20 'blanc) #t)

(check-match (cambiarColorFicha tab empty) tab)

(check-match (heuristica tab 'blanc) 0)

(check-match (getColor tab 0) "brown")

(check-eq? (realizarJugada tab 0 'blanc) #f)

(check-match (realizarJugadaCpu tab 0 'blanc) empty)

(check-match (findLegalPos tab 'blanc) '(20 29 34 43))

(check-eq? (final? tab 'blanc) #f)

(check-match (alphaB tab 'blanc -inf.0 +inf.0 0) '(0 -1))

(check-match (minimax tab 'blanc 0) '(0 -1))

