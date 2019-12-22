#lang Racket
(define tableroInicial
  (list
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
   'blanc 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre 
   'libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre  
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre))

(define (imprimirTablero tablero)
  (for ([i (in-range 0 64)])
    (cond
      [(equal? (remainder i 8) 0) (display "\n") (display (list-ref tablero i))]
      [else (display " ") (display (list-ref tablero i))])))
      
(define (cambiarFicha tablero pos color)
  (cond
    [(equal? (list-ref tablero pos) 'libre) (list-set tablero pos color)]
    [else #f]
    ))

(define (cambiarColor tablero pos)
  (cond
    [(equal? (list-ref tablero pos) 'blanc) 'negra]
    [else 'blanc]))

(define (comprobarColor tablero pos)
  (let* [(up   ((lambda (pos) (cond [(> pos 7)  (- pos 8)] [else pos])) pos))
         (drup ((lambda (pos) (cond [(> pos 0)  (- pos 1)] [else pos])) up))
         (dlup ((lambda (pos) (cond [(< pos 64) (+ pos 1)] [else pos])) up))
         (dw   ((lambda (pos) (cond [(< pos 56) (+ pos 8)] [else pos])) pos))
         (drdw ((lambda (pos) (cond [(> pos 0)  (- pos 1)] [else pos])) dw))
         (dldw ((lambda (pos) (cond [(< pos 64) (+ pos 1)] [else pos])) dw))
         (rgt  ((lambda (pos) (cond [(> pos 0)  (- pos 1)] [else pos])) pos))
         (lft  ((lambda (pos) (cond [(> pos 64) (+ pos 1)] [else pos])) pos))] 
    (cond
      [(or (equal? (list-ref tablero up) (list-ref tablero dw)) (equal? (list-ref tablero rgt) (list-ref tablero lft))) (cambiarColor tablero pos)]
      [else (list-ref tablero pos)])))



(define (comprobarJugada tablero pos)
  (list-set tablero pos (comprobarColor tablero pos)))


(define (realizarJugada tablero posiciones)
  (cond
    [(empty? posiciones) tablero]
    [else (realizarJugada (comprobarJugada tablero (car posiciones)) (cdr posiciones))]))

(define (jugada tablero pos color)
   (let* [(up   ((lambda (pos) (cond [(> pos 7)  (- pos 8)] [else pos])) pos))
         (drup ((lambda (pos)  (cond [(> pos 0)  (- pos 1)] [else pos])) up))
         (dlup ((lambda (pos)  (cond [(< pos 64) (+ pos 1)] [else pos])) up))
         (dw   ((lambda (pos)  (cond [(< pos 56) (+ pos 8)] [else pos])) pos))
         (drdw ((lambda (pos)  (cond [(> pos 0)  (- pos 1)] [else pos])) dw))
         (dldw ((lambda (pos)  (cond [(< pos 64) (+ pos 1)] [else pos])) dw))
         (rgt  ((lambda (pos)  (cond [(> pos 0)  (- pos 1)] [else pos])) pos))
         (lft  ((lambda (pos)  (cond [(> pos 64) (+ pos 1)] [else pos])) pos))]
    (realizarJugada (cambiarFicha tablero pos color) (list up drup dlup dw drdw dldw rgt lft))))

(imprimirTablero (jugada tableroInicial 0 'blanc))
