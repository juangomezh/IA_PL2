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
   'libre 'libre 'libre 'negra 'libre 'libre 'libre 'libre 
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
    [else tablero]
    ))

(define (cambiarColor color)
  (cond
    [(equal? color 'blanc) 'negra]
    [else 'blanc]))


(define (outOfRange pos)
  (or (> pos 63) (< pos 0)))

(define (outOfRangeFila pos)
  (equal? (remainder pos 8) 0))

(define (voltear? tablero pos color step)
  (cond
    [(not (outOfRange (+ pos step)))
     (and
      (equal? (list-ref tablero (+ pos step)) (cambiarColor color))
      (not (boolean? (flip-piece tablero (+ pos step) color step))))]
    [else #f]))

(define (voltear tablero pos color step)
  (for/list ([i (in-range pos (flip-piece tablero (+ pos step) color step) step)])
    i))

(define (flip-piece tablero pos color step)
  (cond
   [(or (equal? step 1) (equal? step -1))
    (cond
     [(outOfRangeFila pos)
       (cond
         [(equal? (list-ref tablero pos) color) pos]
         [else #f])]
      [(equal? (list-ref tablero pos) color) pos]
      [(equal? (list-ref tablero pos) (cambiarColor color)) (flip-piece tablero (+ pos step) color step)]
      [else #f])]
    [else
     (cond
      [(outOfRange pos) #f]
      [(equal? (list-ref tablero pos) color) pos]
      [(equal? (list-ref tablero pos) (cambiarColor color)) (flip-piece tablero (+ pos step) color step)]
      [else #f])]))

(define (comprobarAlrededor tablero posiciones)
  
  (for/or ([i posiciones]
           #:when (> i 0))
    (not (equal? (list-ref tablero i) 'libre))))

(define (movimientoLegal tablero pos color)
  
   (let* [(up   ((lambda (pos) (cond [(> pos 7)  (- pos 8)] [else -1])) pos))
         (drup ((lambda (pos) (cond [(< pos 0) -1] [(> pos 0)  (- pos 1)] [else -1])) up))
         (dlup ((lambda (pos) (cond [(< pos 0) -1] [(< pos 63) (+ pos 1)] [else -1])) up))
         (dw   ((lambda (pos) (cond [(< pos 56) (+ pos 8)] [else -1])) pos))
         (drdw ((lambda (pos) (cond [(< pos 0) -1] [(> pos 0)  (- pos 1)] [else -1])) dw))
         (dldw ((lambda (pos) (cond [(< pos 0) -1] [(< pos 63) (+ pos 1)] [else -1])) dw))
         (rgt  ((lambda (pos) (cond [(> (remainder pos 8) 0)  (- pos 1)] [else -1])) pos))
         (lft  ((lambda (pos) (cond [(< (remainder pos 8) 7) (+ pos 1)] [else -1])) pos))]
    (and (comprobarAlrededor (cambiarFicha tablero pos color) (list up drup dlup dw drdw dldw rgt lft))
         (or
          (voltear? tablero pos color -1)
          (voltear? tablero pos color +1)
          (voltear? tablero pos color -8)
          (voltear? tablero pos color +8)
          (voltear? tablero pos color -9)
          (voltear? tablero pos color +9)
          (voltear? tablero pos color -7)
          (voltear? tablero pos color +7))
         )))
  

(define (cambiarColorFicha tablero lista)
  (cond
    [(empty? lista) tablero]
    [else
     (cambiarColorFicha (list-set tablero (car lista) (cambiarColor (list-ref tablero (car lista)))) (cdr lista))]
    ))
    

(define (cambiarFichas tablero pos color)
  (set! tablero (list-set tablero pos (cambiarColor color)))
  (when (voltear? tablero pos color -1)
    (let ([fichas (voltear tablero pos color -1)])
      (set! tablero (cambiarColorFicha tablero fichas))))

  (when (voltear? tablero pos color 1)
    (let ([fichas (voltear tablero pos color 1)])
      (set! tablero (cambiarColorFicha tablero fichas))))

  (when (voltear? tablero pos color 8)
    (let ([fichas (voltear tablero pos color 8)])
      (set! tablero (cambiarColorFicha tablero fichas)))
    )
  
  (when (voltear? tablero pos color -8)
    (let ([fichas (voltear tablero pos color -8)])
      (set! tablero (cambiarColorFicha tablero fichas))))

  (when (voltear? tablero pos color 7)
    (let ([fichas (voltear tablero pos color 7)])
      (set! tablero (cambiarColorFicha tablero fichas))))

  (when (voltear? tablero pos color -7)
    (let ([fichas (voltear tablero pos color -7)])
      (set! tablero (cambiarColorFicha tablero fichas))))

  (when (voltear? tablero pos color 9)
    (let ([fichas (voltear tablero pos color 9)])
      (set! tablero (cambiarColorFicha tablero fichas))))

  (when (voltear? tablero pos color -9)
    (let ([fichas (voltear tablero pos color -9)])
      (set! tablero (cambiarColorFicha tablero fichas))))
  
  tablero) 

(define (getColor tablero pos)
  (cond
    [(equal? (list-ref tablero pos) 'blanc) "white"]
    [else "black"]))

(define (realizarJugada tablero pos color)
  (cond
    [(movimientoLegal tablero pos color) (cambiarFichas tablero pos color)]
    [else #f]))

(define (realizarJugadaCpu tablero pos color)
  (cambiarFichas tablero pos color))
;;__________________________________________IA________________________________________________________;;

(define (findLegalPos tablero color)
  (for/list ([i (in-range 0 64)]
             #:when (movimientoLegal tablero i color))
    i))

(define (contarFichas tablero color)
  (let [(contar (for/list ([i (in-range 0 64)]
                           #:when (equal? (list-ref tablero i) color))
                  1))]
    (cond
      [(empty? contar) 0]
      [else
       (foldr (lambda (number1 number2) (+ number1 number2)) 0
             contar)])))

(define (heuristica tablero color)
  (- (contarFichas tablero color) (contarFichas tablero (cambiarColor color))))

(define (final? tablero color)
  (and
   (for/and ([i (in-range 0 64)])
    (not (equal? (list-ref tablero i) 'libre)))
   (empty? (findLegalPos tablero color))))


(define (posibleJugador tablero color)
  (let* [(posiciones (findLegalPos tablero color))]
    (for/list ([i posiciones])
      (realizarJugada tablero i color))))

(define (max-list list) 
    (define (max num1 num2)
    (if (> num1 num2) num1 num2))
    (foldl max (first list) (rest list)))

(define (stepm tablero color)
  (let* [(listaJugadas (posibleJugador tablero color))
         (contadores (for/list [(i listaJugadas)]
                       (heuristica i color)))
         (maximo (max-list contadores))]
    (list-ref listaJugadas (index-of contadores maximo))))

(define (stepminimax tablero color)
  (posibleJugador tablero color))

(define (ganador? tablero color)
  (cond
    [(> (heuristica tablero color) 0) 1]
    [(< (heuristica tablero color) 0) -1]
    [else 0]))

(define (minimax tablero color frontera)
   (cond
    [(= frontera 0)
     (cons (heuristica tablero color) -1)]
    [else
     (let
         [(listaPosiciones (findLegalPos tablero color))
          (listaJugadas (posibleJugador tablero color))]
       (cond
         [(empty? listaJugadas)
          (let
              [(listaJugadasSiguientes (posibleJugador tablero (cambiarColor color)))]
            (cond
              [(empty? listaJugadasSiguientes) (cons -1 -1)]
              [else
               (cons (- (car (minimax tablero (cambiarColor color) (- frontera 1))) -1))]))]
         [else
          (let* [(contadores (for/list [(i listaJugadas)]
                              (- (car (minimax i (cambiarColor color) (- frontera 1))) 0)))
                 (maximo (max-list contadores))
                 (pos (index-of contadores maximo))]
            (cons maximo (list-ref listaPosiciones pos)))]))]))
            
          
    
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



(let* [(jugada (realizarJugada tableroInicial 50 'blanc))
       (jugada2 (realizarJugadaCpu jugada (cdr (minimax jugada 'negra 5)) 'negra))
       (jugada3 (realizarJugada jugada2 20 'blanc))
       (jugada4 (realizarJugadaCpu jugada3 (cdr (minimax jugada3 'negra 5)) 'negra))]
  (displayTablero jugada4))

