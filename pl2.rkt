#lang Racket
(require 2htdp/universe)
(require 2htdp/image)
(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define ventana (open-viewport "TAB" 500 500))
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

(define (cambiarColor color)
  (cond
    [(equal? color 'blanc) 'negra]
    [else 'blanc]))

(define (comprobarColor tablero pos)
  (let* [(tab tablero)
         (up   ((lambda (pos) (cond [(> pos 7)  (- pos 8)] [else -1])) pos))
         (drup ((lambda (pos) (cond [(< pos 0) -1] [(> pos 0)  (- pos 1)] [else -1])) up))
         (dlup ((lambda (pos) (cond [(< pos 0) -1] [(< pos 64) (+ pos 1)] [else -1])) up))
         (dw   ((lambda (pos) (cond [(< pos 56) (+ pos 8)] [else -1])) pos))
         (drdw ((lambda (pos) (cond [(< pos 0) -1] [(> pos 0)  (- pos 1)] [else -1])) dw))
         (dldw ((lambda (pos) (cond [(< pos 0) -1] [(< pos 64) (+ pos 1)] [else -1])) dw))
         (rgt  ((lambda (pos) (cond [(> (remainder pos 8) 0)  (- pos 1)] [else -1])) pos))
         (lft  ((lambda (pos) (cond [(< (remainder pos 8) 7) (+ pos 1)] [else -1])) pos))] 

    (cond
     [(and (not (< up 0)) (not (< dw 0)) (not (< rgt 0)) (not (< lft 0)) (not (< drup 0)) (not (< drdw 0)) (not (< dlup 0)) (not (< dldw 0)))
        (cond
          [(and (equal? (list-ref tablero up) (list-ref tablero dw)) (not (equal? (list-ref tablero pos) 'libre))) 
            (cambiarColor tab pos)]
          [(and (equal? (list-ref tablero lft) (list-ref tablero rgt)) (not (equal? (list-ref tablero pos) 'libre)))
            (cambiarColor tab pos)]
          [(and (equal? (list-ref tablero drup) (list-ref tablero dldw)) (not (equal? (list-ref tablero pos) 'libre))) 
            (cambiarColor tab pos)]
          [(and (equal? (list-ref tablero dlup) (list-ref tablero drdw)) (not (equal? (list-ref tablero pos) 'libre))) 
            (cambiarColor tab pos)]
          [else (list-ref tablero pos)])]
     [(and (not (< up 0)) (not (< dw 0)))
      (cond
          [(and (equal? (list-ref tablero up) (list-ref tablero dw)) (not (equal? (list-ref tablero pos) 'libre))) 
            (cambiarColor tab pos)]
          [else (list-ref tablero pos)])]
     [(and (not (< drup 0)) (not (< dldw 0)))
      (cond
          [(and (equal? (list-ref tablero drup) (list-ref tablero dldw)) (not (equal? (list-ref tablero pos) 'libre))) 
            (cambiarColor tab pos)]
          [else (list-ref tablero pos)])]
     [(and (not (< dlup 0)) (not (< drdw 0)))
      (cond
          [(and (equal? (list-ref tablero dlup) (list-ref tablero drdw)) (not (equal? (list-ref tablero pos) 'libre))) 
            (cambiarColor tab pos)]
          [else (list-ref tablero pos)])]
     [(and (not (< rgt 0)) (not (< lft 0)))
      (cond
        [(and (equal? (list-ref tablero lft) (list-ref tablero rgt)) (not (equal? (list-ref tablero pos) 'libre)))
            (cambiarColor tab pos)]
          [else (list-ref tablero pos)])]
       
     [else (list-ref tablero pos)])))



(define (comprobarJugada tablero pos)
  (list-set tablero pos (comprobarColor tablero pos)))

(define (flip-piece tablero pos color step)
  (cond
   [(or (equal? step 1) (equal? step -1))
    (cond
      [(outOfRangeFila pos)
       (cond
         [(equal? (list-ref tablero pos) (cambiarClor color)) pos]
         [else #f])]
      [(equal? (list-ref tablero pos) color) pos]
      [(equal? (list-ref tablero pos) (cambiarClor color)) (flip-piece tablero (+ pos step) color step)]
      [else #f])
    [else
     (cond
      [(outOfRange pos) #f]
      [(equal? (list-ref tablero pos) color) pos]
      [(equal? (list-ref tablero pos) (cambiarClor color)) (flip-piece tablero (+ pos step) color step)]
      [else #f])]]))

(define (outOfRange pos)
  (or (> pos 64) (< pos 0)))

(define (outOfRangeFila pos)
  (equal (remainder pos 8) 0))

(define (voltear? tablero pos color step)
  (and
   (equal? (list-ref tablero (+ pos step)) (cambiarColor color))
   (flip-piece tablero pos 'blanc step)))

(define (comprobarAlrededor tablero posiciones)
  (for/or ([i posiciones]
           #:when (> i 0))
    (not (equal? (list-ref tablero i) 'libre))))

(define (movimientoLegal tablero pos color)
   (let* [(up   ((lambda (pos) (cond [(> pos 7)  (- pos 8)] [else -1])) pos))
         (drup ((lambda (pos) (cond [(< pos 0) -1] [(> pos 0)  (- pos 1)] [else -1])) up))
         (dlup ((lambda (pos) (cond [(< pos 0) -1] [(< pos 64) (+ pos 1)] [else -1])) up))
         (dw   ((lambda (pos) (cond [(< pos 56) (+ pos 8)] [else -1])) pos))
         (drdw ((lambda (pos) (cond [(< pos 0) -1] [(> pos 0)  (- pos 1)] [else -1])) dw))
         (dldw ((lambda (pos) (cond [(< pos 0) -1] [(< pos 64) (+ pos 1)] [else -1])) dw))
         (rgt  ((lambda (pos) (cond [(> (remainder pos 8) 0)  (- pos 1)] [else -1])) pos))
         (lft  ((lambda (pos) (cond [(< (remainder pos 8) 7) (+ pos 1)] [else -1])) pos))] 
    (and (comprobarAlrededor (cambiarFicha tablero pos color) (list up drup dlup dw drdw dldw rgt lft))
         (voltear? tablero pos color -1)
         (voltear? tablero pos color +1)
         (voltear? tablero pos color -8)
         (voltear? tablero pos color +8)
         (voltear? tablero pos color -9)
         (voltear? tablero pos color +9)
         (voltear? tablero pos color -7)
         (voltear? tablero pos color +7)
         )))

(define (getColor tablero pos)
  (cond
    [(equal? (list-ref tablero pos) 'blanc) "white"]
    [else "black"]))
          
(define (displayTablero tablero)
  ((draw-solid-rectangle ventana)	 	 	 	 
   (make-posn 50 50)	 	 	 	 
   400	 	 	 	 
   400	 	 	 	 
   "brown")
  (for ([i (in-range 100 450 50)]) 
    ((draw-line ventana) (make-posn 50 i) (make-posn 450 i) "black"))
  (for ([i (in-range 100 450 50)]) 
    ((draw-line ventana) (make-posn i 50) (make-posn i 450) "black"))
  (for ([i (in-range 0 64)])
    (when (not (equal? (list-ref tablero i) 'libre))
      ((draw-solid-ellipse ventana)
       (let [(x (+ 60 (* 50 (remainder i 8))))
             (y (+ 60 (* 50 (quotient i 8))))]
         (make-posn x y))	 	 	 	 
       30	 	 	 	 
       30	 	 	 	 
       (getColor tablero i)))))

(displayTablero tableroInicial)
(provide big-bang)
