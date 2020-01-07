#lang Racket
(require 2htdp/universe)
(require 2htdp/image)
(require graphics/graphics)
(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define opcion 0)

(define (elegirOpcion op)
  (set! opcion op))

(define tab
  (list
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre  
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'blanc 'negra 'libre 'libre 'libre 
   'libre 'libre 'libre 'negra 'blanc 'libre 'libre 'libre  
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre 
   'libre 'libre 'libre 'libre 'libre 'libre 'libre 'libre))

(define fila1 '(0 1 2 3 4 5 6 7))
(define columna1 '(0 8 16 24 32 40 48 56))
(define columna2 '(7 15 23 31 39 47 55 62))                   
(define fila2 '(56 57 58 59 60 61 62 63))

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
    [(equal? color 'negra) 'blanc]
    [else 'libre]))

(define (voltear? tablero pos color step)
  (cond
    [(not (outOfRange (+ pos step)))
     (and
      (equal? (list-ref tablero (+ pos step)) (cambiarColor color))
      (not (boolean? (flip-piece tablero (+ pos step) color step))))]
    [else #f]))

(define (voltear tablero pos color step)
  (for/list ([i (in-range (+ pos step) (flip-piece tablero (+ pos step) color step) step)])
    i))

(define (outOfRange pos)
  (or (> pos 63) (< pos 0)))


(define (outOfRangeFila pos)
  (or (not (boolean? (member pos columna1))) (not (boolean? (member pos columna2)))))

(define (outOfRangeColumna pos)
  (or (not (boolean? (member pos fila1))) (not (boolean? (member pos fila2)))))

(define (outOfRangeDiagonal pos)
  (or (not (boolean? (member pos fila1))) (not (boolean? (member pos fila2)))
      (not (boolean? (member pos columna1))) (not (boolean? (member pos columna2)))))


(define (flip-piece tablero pos color step)
  (cond
   [(or (equal? step 1) (equal? step -1))
    (cond
     [(outOfRange pos) #f]
     [(outOfRangeFila pos)
       (cond
         [(equal? (list-ref tablero pos) color) pos]
         [else #f])]
      [(equal? (list-ref tablero pos) color) pos]
      [(equal? (list-ref tablero pos) (cambiarColor color)) (flip-piece tablero (+ pos step) color step)]
      [else #f])]
   [(or (equal? step 8) (equal? step -8))
    (cond
      [(outOfRange pos) #f]
      [(outOfRangeColumna pos)
       (cond
         [(equal? (list-ref tablero pos) color) pos]
         [else #f])]
      [(equal? (list-ref tablero pos) color) pos]
      [(equal? (list-ref tablero pos) (cambiarColor color)) (flip-piece tablero (+ pos step) color step)]
      [else #f])]
    [else
     (cond
      [(outOfRange pos) #f]
      [(outOfRangeDiagonal pos)
       (cond
         [(equal? (list-ref tablero pos) color) pos]
         [else #f])]
      [(equal? (list-ref tablero pos) color) pos]
      [(equal? (list-ref tablero pos) (cambiarColor color)) (flip-piece tablero (+ pos step) color step)]
      [else #f])]))

(define (comprobarAlrededor tablero posiciones)
  
  (for/or ([i posiciones]
           #:when (> i 0))
    (not (equal? (list-ref tablero i) 'libre))))

(define (movimientoLegal tablero pos color)
  
   (let* [(up  ((lambda (pos) (cond [(> pos 7)  (- pos 8)] [else -1])) pos))
         (drup ((lambda (pos) (cond [(< pos 0) -1] [(> pos 0)  (- pos 1)] [else -1])) up))
         (dlup ((lambda (pos) (cond [(< pos 0) -1] [(< pos 63) (+ pos 1)] [else -1])) up))
         (dw   ((lambda (pos) (cond [(< pos 56) (+ pos 8)] [else -1])) pos))
         (drdw ((lambda (pos) (cond [(< pos 0) -1] [(> pos 0)  (- pos 1)] [else -1])) dw))
         (dldw ((lambda (pos) (cond [(< pos 0) -1] [(< pos 63) (+ pos 1)] [else -1])) dw))
         (rgt  ((lambda (pos) (cond [(> (remainder pos 8) 0)  (- pos 1)] [else -1])) pos))
         (lft  ((lambda (pos) (cond [(< (remainder pos 8) 7) (+ pos 1)] [else -1])) pos))]
    (and
     (equal? (list-ref tablero pos) 'libre)
     (comprobarAlrededor (cambiarFicha tablero pos color) (list up drup dlup dw drdw dldw rgt lft))
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
  (set! tablero (list-set tablero pos color))
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
    [(equal? (list-ref tablero pos) 'negra) "black"]
    [else "brown"]))

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
  (or
   (for/and ([i (in-range 0 64)])
    (not (equal? (list-ref tablero i) 'libre)))
   (empty? (findLegalPos tablero color))
   (empty? (findLegalPos tablero (cambiarColor color)))))


(define (posibleJugador tablero color)
  (let* [(posiciones (findLegalPos tablero color))]
    (for/list ([i posiciones])
      (realizarJugada tablero i color))))

(define (max-list list) 
    (define (max num1 num2)
    (if (> num1 num2) num1 num2))
    (foldl max (first list) (rest list)))

(define (maxAlpha list valor) 
    (define (max num1 num2)
      (if (> num1 num2) num1 num2))
    (foldl max valor list))

(define (alphaB tablero color alpha beta frontera)
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
               (cons (- (car (alphaB tablero (cambiarColor color) (- beta) (- alpha) (- frontera 1)))) -1)]))]
         [else
          (let* [(contadores (for/list [(i listaJugadas)] #:break (>= alpha beta)
                              (let [(valor (- (car (alphaB tablero (cambiarColor color) (- beta) (- alpha) (- frontera 1))) -1))]
                                (when (> valor alpha) (set! alpha valor)) valor)))
                 (maximo (max-list contadores))
                 (pos (index-of contadores maximo))]
            (cons maximo (list-ref listaPosiciones pos)))]))]))

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
               (cons (- (car (minimax tablero (cambiarColor color) (- frontera 1)))) -1)]))]
         [else
          (let* [(contadores (for/list [(i listaJugadas)]
                              (- (car (minimax i (cambiarColor color) (- frontera 1))) 0)))
                 (maximo (max-list contadores))
                 (pos (index-of contadores maximo))]
            (cons maximo (list-ref listaPosiciones pos)))]))]))
           

(define (jugarIA)
  (cond
    [(equal? opcion 0)
     (cond
       [(final? tab 'negra) (display "Fin del juego")]
       [(not (empty? (findLegalPos tab 'negra)))
        (let [(jugadaCpu (realizarJugadaCpu tab (cdr (alphaB tab 'negra -inf.0 +inf.0 5)) 'negra))]
          (cond
            [(not (boolean? jugadaCpu))
             (set! tab jugadaCpu)]
            [else (display "No puedo seguir...")]))]
       [else (display "No puedo seguir...")])
     (displayTablero tab)
     (seguir (findLegalPos tab 'blanc))
     ]
    [else
     (cond
       [(final? tab 'negra) (display "Fin del juego")]
       [(not (empty? (findLegalPos tab 'negra)))
        (let [(jugadaCpu (realizarJugadaCpu tab (cdr (minimax tab 'negra 5)) 'negra))]
          (cond
            [(not (boolean? jugadaCpu))
             (set! tab jugadaCpu)]
            [else (display "No puedo seguir...")]))]
       [else (display "No puedo seguir...")])
     (displayTablero tab)
     
     (seguir (findLegalPos tab 'blanc))]
    ))

(define (seguir lista)
  (cond
    [(and (empty? lista) (empty? (findLegalPos tab 'negra))) ((draw-string ventana) (make-posn 200 30) "FIN DEL JUEGO " "black")]
    [(empty? (findLegalPos tab 'negra)) ((draw-string ventana) (make-posn 200 30) "Continua tu..." "black")]
    [(empty? lista) ((draw-string ventana) (make-posn 200 30) "Tu no puedes, sigo yo" "black")
                    (jugarIA)]
    [else ""])
    )

(define (jugar pos)
  (cond
    [(not (empty? (findLegalPos tab 'blanc)))
    (let [(jugada (realizarJugada tab pos 'blanc))]
      (cond
        [(not (boolean? jugada))
              (set! tab jugada)
              (displayTablero tab)
              ((draw-solid-rectangle ventana)	 	 	 	 
               (make-posn 350 450)	150 50 "white")
              ((draw-string ventana) (make-posn 400 480) "MI TURNO" "black")
              (jugarIA)]
        [else ((draw-string ventana) (make-posn 180 30) "Jugada erronea - Repita con otra de las posiciones indicadas" "black")]))]
    [else ((draw-string ventana) (make-posn 200 30) "FIN DEL JUEGO " "black")])
  (display "\n")
  ((draw-solid-rectangle ventana)	 	 	 	 
   (make-posn 0 450)	500 50 "white")
  
  ((draw-string ventana) (make-posn 400 480) "TU TURNO " "black")
  ((draw-string ventana) (make-posn 10 480) "posibles soluciones: " "black")
  (dibujarPosibles (findLegalPos tab 'blanc) 150))
                   






;;----------------------------------------------------------------Interzaf---------------------------------------------------------------;;


(define click_inicial null)

(define (draw mouse)
  (cond

;-----------------------------------------------------------------------Dibujar Casillas A------------------------------------------------------------------------------------------------------------------------    
    ;Casilla a1
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 50 )(<= ( posn-x(mouse-click-posn mouse)) 100 ) (>= ( posn-y(mouse-click-posn mouse)) 50 )(<= ( posn-y(mouse-click-posn mouse)) 100 ))
      (jugar 0)]

         ;Casilla a2
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 100 )(<= ( posn-x(mouse-click-posn mouse)) 150 ) (>= ( posn-y(mouse-click-posn mouse)) 50 )(<= ( posn-y(mouse-click-posn mouse)) 100 ))
      (jugar 1)]

        ;Casilla a3
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 150 )(<= ( posn-x(mouse-click-posn mouse)) 200 ) (>= ( posn-y(mouse-click-posn mouse)) 50 )(<= ( posn-y(mouse-click-posn mouse)) 100 ))
      (jugar 2)]
     
      ;Casilla a4
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 200 )(<= ( posn-x(mouse-click-posn mouse)) 250 ) (>= ( posn-y(mouse-click-posn mouse)) 50 )(<= ( posn-y(mouse-click-posn mouse)) 100 ))
      (jugar 3)]

        ;Casilla a5
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 250 )(<= ( posn-x(mouse-click-posn mouse)) 300 ) (>= ( posn-y(mouse-click-posn mouse)) 50 )(<= ( posn-y(mouse-click-posn mouse)) 100 ))
      (jugar 4)]

        ;Casilla a6
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 300 )(<= ( posn-x(mouse-click-posn mouse)) 350 ) (>= ( posn-y(mouse-click-posn mouse)) 50 )(<= ( posn-y(mouse-click-posn mouse)) 100 ))
      (jugar 5)]

     
      ;Casilla a7
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 350 )(<= ( posn-x(mouse-click-posn mouse)) 400 ) (>= ( posn-y(mouse-click-posn mouse)) 50 )(<= ( posn-y(mouse-click-posn mouse)) 100 ))
      (jugar 6)]

        ;Casilla a8
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 400 )(<= ( posn-x(mouse-click-posn mouse)) 450 ) (>= ( posn-y(mouse-click-posn mouse)) 50 )(<= ( posn-y(mouse-click-posn mouse)) 100 ))
      (jugar 7)]
;------------------------------------------------------------------------------Dibujar Casillas B--------------------------------------------------------------------------------------------------------------
      ;Casilla b1
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 50 )(<= ( posn-x(mouse-click-posn mouse)) 100 ) (>= ( posn-y(mouse-click-posn mouse)) 100 )(<= ( posn-y(mouse-click-posn mouse)) 150 ))
      (jugar 8)]

         ;Casilla b2
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 100 )(<= ( posn-x(mouse-click-posn mouse)) 150 ) (>= ( posn-y(mouse-click-posn mouse)) 100 )(<= ( posn-y(mouse-click-posn mouse)) 150 ))
      (jugar 9)]

        ;Casilla b3
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 150 )(<= ( posn-x(mouse-click-posn mouse)) 200 ) (>= ( posn-y(mouse-click-posn mouse)) 100 )(<= ( posn-y(mouse-click-posn mouse)) 150 ))
      (jugar 10)]
     
      ;Casilla b4
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 200 )(<= ( posn-x(mouse-click-posn mouse)) 250 ) (>= ( posn-y(mouse-click-posn mouse)) 100 )(<= ( posn-y(mouse-click-posn mouse)) 150 ))
      (jugar 11)]

        ;Casilla b5
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 250 )(<= ( posn-x(mouse-click-posn mouse)) 300 ) (>= ( posn-y(mouse-click-posn mouse)) 100 )(<= ( posn-y(mouse-click-posn mouse)) 150 ))
      (jugar 12)]

        ;Casilla b6
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 300 )(<= ( posn-x(mouse-click-posn mouse)) 350 ) (>= ( posn-y(mouse-click-posn mouse)) 100 )(<= ( posn-y(mouse-click-posn mouse)) 150 ))
      (jugar 13)]

     
      ;Casilla b7
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 350 )(<= ( posn-x(mouse-click-posn mouse)) 400 ) (>= ( posn-y(mouse-click-posn mouse)) 100 )(<= ( posn-y(mouse-click-posn mouse)) 150 ))
      (jugar 14)]

        ;Casilla b8
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 400 )(<= ( posn-x(mouse-click-posn mouse)) 450 ) (>= ( posn-y(mouse-click-posn mouse)) 100 )(<= ( posn-y(mouse-click-posn mouse)) 150 ))
      (jugar 15)]

     ;------------------------------------------------------------------------------Dibujar Casillas C--------------------------------------------------------------------------------------------------------------
      ;Casilla c1
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 50 )(<= ( posn-x(mouse-click-posn mouse)) 100 ) (>= ( posn-y(mouse-click-posn mouse)) 150 )(<= ( posn-y(mouse-click-posn mouse)) 200 ))
      (jugar 16)]

         ;Casilla c2
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 100 )(<= ( posn-x(mouse-click-posn mouse)) 150 ) (>= ( posn-y(mouse-click-posn mouse)) 150 )(<= ( posn-y(mouse-click-posn mouse)) 200 ))
      (jugar 17)]

        ;Casilla c3
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 150 )(<= ( posn-x(mouse-click-posn mouse)) 200 ) (>= ( posn-y(mouse-click-posn mouse)) 150 )(<= ( posn-y(mouse-click-posn mouse)) 200 ))
      (jugar 18)]
     
      ;Casilla c4
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 200 )(<= ( posn-x(mouse-click-posn mouse)) 250 ) (>= ( posn-y(mouse-click-posn mouse)) 150 )(<= ( posn-y(mouse-click-posn mouse)) 200 ))
      (jugar 19)]

        ;Casilla c5
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 250 )(<= ( posn-x(mouse-click-posn mouse)) 300 ) (>= ( posn-y(mouse-click-posn mouse)) 150 )(<= ( posn-y(mouse-click-posn mouse)) 200 ))
      (jugar 20)]

        ;Casilla c6
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 300 )(<= ( posn-x(mouse-click-posn mouse)) 350 ) (>= ( posn-y(mouse-click-posn mouse)) 150 )(<= ( posn-y(mouse-click-posn mouse)) 200 ))
      (jugar 21)]

     
      ;Casilla c7
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 350 )(<= ( posn-x(mouse-click-posn mouse)) 400 ) (>= ( posn-y(mouse-click-posn mouse)) 150 )(<= ( posn-y(mouse-click-posn mouse)) 200 ))
      (jugar 22)]

        ;Casilla c8
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 400 )(<= ( posn-x(mouse-click-posn mouse)) 450 ) (>= ( posn-y(mouse-click-posn mouse)) 150 )(<= ( posn-y(mouse-click-posn mouse)) 200 ))
      (jugar 23)]

  ;------------------------------------------------------------------------------Dibujar Casillas D--------------------------------------------------------------------------------------------------------------
      ;Casilla d1
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 50 )(<= ( posn-x(mouse-click-posn mouse)) 100 ) (>= ( posn-y(mouse-click-posn mouse)) 200 )(<= ( posn-y(mouse-click-posn mouse)) 250 ))
      (jugar 24)]

         ;Casilla d2
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 100 )(<= ( posn-x(mouse-click-posn mouse)) 150 ) (>= ( posn-y(mouse-click-posn mouse)) 200 )(<= ( posn-y(mouse-click-posn mouse)) 250 ))
      (jugar 25)]

        ;Casilla d3
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 150 )(<= ( posn-x(mouse-click-posn mouse)) 200 ) (>= ( posn-y(mouse-click-posn mouse)) 200 )(<= ( posn-y(mouse-click-posn mouse)) 250 ))
      (jugar 26)]
     
      ;Casilla d4
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 200 )(<= ( posn-x(mouse-click-posn mouse)) 250 ) (>= ( posn-y(mouse-click-posn mouse)) 200 )(<= ( posn-y(mouse-click-posn mouse)) 250 ))
      (jugar 27)]

        ;Casilla d5
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 250 )(<= ( posn-x(mouse-click-posn mouse)) 300 ) (>= ( posn-y(mouse-click-posn mouse)) 200 )(<= ( posn-y(mouse-click-posn mouse)) 250 ))
      (jugar 28)]

        ;Casilla d6
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 300 )(<= ( posn-x(mouse-click-posn mouse)) 350 ) (>= ( posn-y(mouse-click-posn mouse)) 200 )(<= ( posn-y(mouse-click-posn mouse)) 250 ))
      (jugar 29)]

     
      ;Casilla d7
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 350 )(<= ( posn-x(mouse-click-posn mouse)) 400 ) (>= ( posn-y(mouse-click-posn mouse)) 200 )(<= ( posn-y(mouse-click-posn mouse)) 250 ))
      (jugar 30)]

        ;Casilla d8
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 400 )(<= ( posn-x(mouse-click-posn mouse)) 450 ) (>= ( posn-y(mouse-click-posn mouse)) 200 )(<= ( posn-y(mouse-click-posn mouse)) 250 ))
      (jugar 31)]

 ;------------------------------------------------------------------------------Dibujar Casillas E--------------------------------------------------------------------------------------------------------------
      ;Casilla e1
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 50 )(<= ( posn-x(mouse-click-posn mouse)) 100 ) (>= ( posn-y(mouse-click-posn mouse)) 250 )(<= ( posn-y(mouse-click-posn mouse)) 300 ))
      (jugar 32)]

         ;Casilla e2
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 100 )(<= ( posn-x(mouse-click-posn mouse)) 150) (>= ( posn-y(mouse-click-posn mouse)) 250 )(<= ( posn-y(mouse-click-posn mouse)) 300 ))
      (jugar 33)]

        ;Casilla e3
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 150 )(<= ( posn-x(mouse-click-posn mouse)) 200 ) (>= ( posn-y(mouse-click-posn mouse)) 250 )(<= ( posn-y(mouse-click-posn mouse)) 300 ))
      (jugar 34)]
     
      ;Casilla e4
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 200 )(<= ( posn-x(mouse-click-posn mouse)) 250 ) (>= ( posn-y(mouse-click-posn mouse)) 250 )(<= ( posn-y(mouse-click-posn mouse)) 300 ))
      (jugar 35)]

        ;Casilla e5
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 250 )(<= ( posn-x(mouse-click-posn mouse)) 300 ) (>= ( posn-y(mouse-click-posn mouse)) 250 )(<= ( posn-y(mouse-click-posn mouse)) 300 ))
      (jugar 36)]

        ;Casilla e6
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 300 )(<= ( posn-x(mouse-click-posn mouse)) 350 ) (>= ( posn-y(mouse-click-posn mouse)) 250 )(<= ( posn-y(mouse-click-posn mouse)) 300 ))
      (jugar 37)]

     
      ;Casilla e7
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 350 )(<= ( posn-x(mouse-click-posn mouse)) 400 ) (>= ( posn-y(mouse-click-posn mouse)) 250 )(<= ( posn-y(mouse-click-posn mouse)) 300 ))
      (jugar 38)]

        ;Casilla e8
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 400 )(<= ( posn-x(mouse-click-posn mouse)) 450 ) (>= ( posn-y(mouse-click-posn mouse)) 250 )(<= ( posn-y(mouse-click-posn mouse)) 300 ))
      (jugar 39)]

  ;------------------------------------------------------------------------------Dibujar Casillas F--------------------------------------------------------------------------------------------------------------
      ;Casilla f1
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 50 )(<= ( posn-x(mouse-click-posn mouse)) 100 ) (>= ( posn-y(mouse-click-posn mouse)) 300 )(<= ( posn-y(mouse-click-posn mouse)) 350 ))
      (jugar 40)]

         ;Casilla f2
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 100 )(<= ( posn-x(mouse-click-posn mouse)) 150 ) (>= ( posn-y(mouse-click-posn mouse)) 300 )(<= ( posn-y(mouse-click-posn mouse)) 350 ))
      (jugar 41)]

        ;Casilla f3
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 150 )(<= ( posn-x(mouse-click-posn mouse)) 200 ) (>= ( posn-y(mouse-click-posn mouse)) 300 )(<= ( posn-y(mouse-click-posn mouse)) 350 ))
      (jugar 42)]
     
      ;Casilla e4
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 200 )(<= ( posn-x(mouse-click-posn mouse)) 250 ) (>= ( posn-y(mouse-click-posn mouse)) 300 )(<= ( posn-y(mouse-click-posn mouse)) 350 ))
      (jugar 43)]

        ;Casilla f5
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 250 )(<= ( posn-x(mouse-click-posn mouse)) 300 ) (>= ( posn-y(mouse-click-posn mouse)) 300 )(<= ( posn-y(mouse-click-posn mouse)) 350 ))
      (jugar 44)]

        ;Casilla f6
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 300 )(<= ( posn-x(mouse-click-posn mouse)) 350 ) (>= ( posn-y(mouse-click-posn mouse)) 300 )(<= ( posn-y(mouse-click-posn mouse)) 350 ))
      (jugar 45)]

     
      ;Casilla f7
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 350 )(<= ( posn-x(mouse-click-posn mouse)) 400 ) (>= ( posn-y(mouse-click-posn mouse)) 300 )(<= ( posn-y(mouse-click-posn mouse)) 350 ))
      (jugar 46)]

        ;Casilla f8
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 400 )(<= ( posn-x(mouse-click-posn mouse)) 450 ) (>= ( posn-y(mouse-click-posn mouse)) 300 )(<= ( posn-y(mouse-click-posn mouse)) 350 ))
      (jugar 47)]

  ;------------------------------------------------------------------------------Dibujar Casillas G--------------------------------------------------------------------------------------------------------------
      ;Casilla G1
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 50 )(<= ( posn-x(mouse-click-posn mouse)) 100 ) (>= ( posn-y(mouse-click-posn mouse)) 350 )(<= ( posn-y(mouse-click-posn mouse)) 400 ))
      (jugar 48)]

         ;Casilla g2
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 100 )(<= ( posn-x(mouse-click-posn mouse)) 150 ) (>= ( posn-y(mouse-click-posn mouse)) 350 )(<= ( posn-y(mouse-click-posn mouse)) 400 ))
      (jugar 49)]

        ;Casilla g3
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 150 )(<= ( posn-x(mouse-click-posn mouse)) 200 ) (>= ( posn-y(mouse-click-posn mouse)) 350 )(<= ( posn-y(mouse-click-posn mouse)) 400 ))
      (jugar 50)]
     
      ;Casilla g4
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 200 )(<= ( posn-x(mouse-click-posn mouse)) 250 ) (>= ( posn-y(mouse-click-posn mouse)) 350 )(<= ( posn-y(mouse-click-posn mouse)) 400 ))
      (jugar 51)]

        ;Casilla g5
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 250 )(<= ( posn-x(mouse-click-posn mouse)) 300 ) (>= ( posn-y(mouse-click-posn mouse)) 350 )(<= ( posn-y(mouse-click-posn mouse)) 400 ))
      (jugar 52)]

        ;Casilla g6
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 300 )(<= ( posn-x(mouse-click-posn mouse)) 350 ) (>= ( posn-y(mouse-click-posn mouse)) 350 )(<= ( posn-y(mouse-click-posn mouse)) 400 ))
      (jugar 53)]

     
      ;Casilla g7
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 350 )(<= ( posn-x(mouse-click-posn mouse)) 400 ) (>= ( posn-y(mouse-click-posn mouse)) 350 )(<= ( posn-y(mouse-click-posn mouse)) 400 ))
      (jugar 54)]

        ;Casilla g8
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 400 )(<= ( posn-x(mouse-click-posn mouse)) 450 ) (>= ( posn-y(mouse-click-posn mouse)) 350 )(<= ( posn-y(mouse-click-posn mouse)) 400 ))
      (jugar 55)]

  ;------------------------------------------------------------------------------Dibujar Casillas H--------------------------------------------------------------------------------------------------------------
;Casilla h1
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 50 )(<= ( posn-x(mouse-click-posn mouse)) 100 ) (>= ( posn-y(mouse-click-posn mouse)) 400 )(<= ( posn-y(mouse-click-posn mouse)) 450 ))
      (jugar 56)]

         ;Casilla h2
    
     [(and(>= (posn-x(mouse-click-posn mouse))  100 )(<= ( posn-x(mouse-click-posn mouse)) 150 ) (>= ( posn-y(mouse-click-posn mouse)) 400 )(<= ( posn-y(mouse-click-posn mouse)) 450 ))
      (jugar 57)]

        ;Casilla g3
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 150 )(<= ( posn-x(mouse-click-posn mouse)) 200 ) (>= ( posn-y(mouse-click-posn mouse)) 400 )(<= ( posn-y(mouse-click-posn mouse)) 450 ))
      (jugar 58)]
     
      ;Casilla g4
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 200 )(<= ( posn-x(mouse-click-posn mouse)) 250 ) (>= ( posn-y(mouse-click-posn mouse)) 400 )(<= ( posn-y(mouse-click-posn mouse)) 450 ))
      (jugar 59)]

        ;Casilla h5
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 250 )(<= ( posn-x(mouse-click-posn mouse)) 300 ) (>= ( posn-y(mouse-click-posn mouse)) 400 )(<= ( posn-y(mouse-click-posn mouse)) 450 ))
      (jugar 60)]

        ;Casilla h6
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 300 )(<= ( posn-x(mouse-click-posn mouse)) 350 ) (>= ( posn-y(mouse-click-posn mouse)) 400 )(<= ( posn-y(mouse-click-posn mouse)) 450 ))
      (jugar 61)]
     
      ;Casilla h7
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 350 )(<= ( posn-x(mouse-click-posn mouse)) 400 ) (>= ( posn-y(mouse-click-posn mouse)) 400 )(<= ( posn-y(mouse-click-posn mouse)) 450 ))
      (jugar 62)]

        ;Casilla h8
    
     [(and(>= (posn-x(mouse-click-posn mouse)) 400 )(<= ( posn-x(mouse-click-posn mouse)) 450 ) (>= ( posn-y(mouse-click-posn mouse)) 400 )(<= ( posn-y(mouse-click-posn mouse)) 450 ))
      (jugar 63)]
;else     
[else 0]
  ))

(define (partida final)
  (cond
    [(not final)
     (set! click_inicial (get-mouse-click ventana))
     (draw click_inicial)
     (partida (final? tab 'blanc))]
    [else
     (cond
       [(> (contarFichas tab 'blanco) (contarFichas tab 'negra)) ((draw-string ventana) (make-posn 350 30) "PERDISTE " "black")]
       [(< (contarFichas tab 'blanco) (contarFichas tab 'negra)) ((draw-string ventana) (make-posn 350 30) "GANASTE " "black")]
       [else ((draw-string ventana) (make-posn 350 30) "EMPATE " "black")])]))

(define (dibujarPosibles lista posx)
  (cond
    [(empty? lista) (display "")]
    [else
     ((draw-string ventana) (make-posn posx 480) (number->string (car lista)) "black")
     (dibujarPosibles (cdr lista) (+ posx 20))]))
   
(define (displayTablero tablero)
  ((draw-solid-rectangle ventana)	 	 	 	 
   (make-posn 0 0)	500 50 "white")
  ((draw-string ventana) (make-posn 10 30) "Blancas: " "black")
  ((draw-string ventana) (make-posn 65 30) (number->string (contarFichas tab 'blanc)) "black")
  ((draw-string ventana) (make-posn 90 30) "Negras: " "black")
  ((draw-string ventana) (make-posn 145 30) (number->string (contarFichas tab 'negra)) "black")
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

;_______________________________________________________________________MAIN____________________________________________________________________________;;
(display "Input: \n    0-AphaBeta\n    >1-Minimax\n")
(let [(a (read-line (current-input-port) 'any))]
  (elegirOpcion a))
(display "\nque empiece el juego...\n")
(define ventana (open-viewport "Othello" 500 500 ))
((draw-solid-rectangle ventana)	 	 	 	 
 (make-posn 0 450)	500 50 "white")
((draw-string ventana) (make-posn 10 480) "posibles soluciones: " "black")
(dibujarPosibles (findLegalPos tab 'blanc) 130)
(displayTablero tab)
(partida #f)
