#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require (lib "graphics.ss" "graphics"))
(open-graphics)
(define ventana (open-viewport "TAB" 500 500))
((draw-rectangle ventana)	 	 	 	 
 	 	(make-posn 50 50)	 	 	 	 
 	 	400	 	 	 	 
 	 	400	 	 	 	 
                "black")
(for ([i (in-range 100 450 50)]) 
((draw-line ventana) (make-posn 50 i) (make-posn 450 i) "black"))
(for ([i (in-range 100 450 50)]) 
((draw-line ventana) (make-posn i 50) (make-posn i 450) "black"))

((draw-solid-ellipse ventana)	 	 	 	 
 	 	(make-posn 60 60)	 	 	 	 
 	 	30	 	 	 	 
 	 	30	 	 	 	 
 	 "black")