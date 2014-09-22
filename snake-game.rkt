#lang racket
(require 2htdp/universe 2htdp/image)
(define WIDTH 600)
(define HEIGHT 600)
(define SEGMENT (circle 10 "solid" "gray"))
(define HEAD (circle 10 "solid" "red"))

(struct snake (direction head segments))
(struct pos (x y))


(define (start)
  (define snk (snake "right" (pos 100 100) (list  (pos 120 100) (pos 140 100)))) 
  (big-bang snk
            (on-tick draw-snake)
            (on-key move-snake)
            (to-draw render-world)
            ))

(define (draw-snake snk)
  snk)

(define (move-snake snk key)
  (define dir (snake-direction snk))  
  (if (equal? (opposite dir) key) snk (cond [(equal? key "right") (add-head-remove-tail snk "right")] 
                                 [(equal? key "left") (add-head-remove-tail snk "left")]
                                 [(equal? key "down") (add-head-remove-tail snk "down")]
                                 [(equal? key "up") (add-head-remove-tail snk "up")])
      )
  )

(define (opposite dir)
  (cond [(equal? dir "right") "left"] 
        [(equal? dir "left") "right"]
        [(equal? dir "down") "up"]
        [(equal? dir "up") "down"])
  )

(define (add-head-remove-tail snk dir)
  (define old-head (snake-head snk))  
  (define x (pos-x old-head))
  (define y (pos-y old-head))
  (define new-head (cond [(equal? dir "right") (pos (+ x 20 ) y)]
                         [(equal? dir "left") (pos (- x 20 ) y)]
                         [(equal? dir "down") (pos x (+ y 20))]
                         [(equal? dir "up") (pos x (- y 20))])
                         )
  (snake dir new-head (cons old-head (remove (snake-segments snk)))))

(define (render-world snk)
(define dir (snake-direction snk))
(define head (snake-head snk))
(define sgmts (snake-segments snk))
(place-image HEAD (pos-x head) (pos-y head) 
   (render-elements sgmts))
      )

(define (render-elements sgmts)
(if (null? sgmts)  (empty-scene WIDTH HEIGHT) 
(place-image SEGMENT (pos-x (car sgmts)) (pos-y (car sgmts)) 
             (render-elements (cdr sgmts)))))
     

(define (list-length list)
  (if (null? list) 0 (+ 1 (list-length (cdr list)))))

