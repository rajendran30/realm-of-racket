#lang racket
(require 2htdp/universe 2htdp/image)

(struct interval (small big count))

(define TEXT-SIZE 10)

(define HELP-TEXT
  (text "Press upper arrow for larger number and lower arrow for smaller number" TEXT-SIZE "blue"))


(define HELP-TEXT2
  (text "Press = when the number is guessed; q to quit" TEXT-SIZE "blue"))

(define COLOR "red")

(define TEXT-X 50)

(define TEXT-UPPER-Y 50)

(define TEXT-LOWER-Y 200)

(define SIZE 30)

(define WIDTH 400)

(define HEIGHT 400)

(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top" 
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))


(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w) ]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (smaller w)
  (interval (interval-small w) (max (interval-small w) (sub1 (guess w))) (add1 (interval-count w))))

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w))) (interval-big w) (add1 (interval-count w))))

(define (guess w)
  (quotient (+ (interval-big w) (interval-small w) ) 2  ))

(define (render w)
  (overlay (text (string-append (number->string (guess w)) "   " (number->string (interval-count w)))SIZE COLOR) MT-SC))

(define (render-last-scene w)
  (overlay (text "END" SIZE COLOR) MT-SC))

(define (single? w)
  (= (interval-small w) (interval-big w) ))

(define (start lower upper)
  (big-bang (interval lower upper 0)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))

(start 10 100)

