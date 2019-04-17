;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A PosnList is one of:
;  - empty
;  - (cons (make-posn Number Number) PosnList)
; A Posn is (make-posn Number Number)
; Note: the posn structure is defined in the Beginning Student Language,
; and looks like:
; (define-struct posn (x y))

;exercise 1

#;
(define (process-PosnList p)
  (cond [(empty?) ...]
        [else ...]))

;exercise 2

(define list1 empty)
(define list2 (cons (make-posn 12 9) list1))
(define list3 (cons (make-posn 5 20) list2))

;exercise 3

;PosnList -> boolean
;many-positive? takes a PosnList and returns true if
;all x coordinates of all Posns in the PosnList are positive
;or if there are no Posns, otherwise returns false
;(many-positive? list2) = true

(define (many-positive? p)
  (cond [(empty? p) true]
        [(= (remainder (posn-x (first p)) 2) 0) (many-positive? (rest p))]
        [else false]))

(check-expect (many-positive? list2) true)
(check-expect (many-positive? list3) false)

;exercise 4

;PosnList -> image
;draw takes a PosnList and draws all the posns
;on an empty-scene
;(draw list2) = (place-image (circle 5 "solid" "black") 12 9 (empty-scene 300 300))

(define (draw p)
  (cond [(empty? p) (empty-scene 300 300)]
        [else (place-image
               (circle 5 "solid" "black") (posn-x (first p)) (posn-y (first p))
               (draw (rest p)))]))

(check-expect (draw list2) (place-image (circle 5 "solid" "black") 12 9 (empty-scene 300 300)))
(check-expect (draw list1) (empty-scene 300 300))

;exercise 5

;PosnList -> PosnList
;move takes a PosnList and makes another PosnList
;where all the y coordinates increase by 1
;(move list2) = (cons (make-posn 12 10) empty))

(define (move p)
  (cond [(empty? p) empty]
        [else (cons (make-posn (posn-x (first p)) (+ 1 (posn-y (first p)))) (move (rest p)))]))

(check-expect (move list2) (cons (make-posn 12 10) empty))
(check-expect (move list3) (cons (make-posn 5 21) (cons (make-posn 12 10) empty)))

;exercise 6

;PosnList KeyEvent -> PosnList
;add takes a PosnList and adds a new Posn
;with randomly made x and y coordinates
;(add list1) = (cons (make-posn (random 301) (random 301)) list1)

(define (add p ke)
  (cons (make-posn (random 301) (random 301)) p))

;exercise 7

(define (run p)
  (big-bang p
            [on-tick move]
            [to-draw draw]
            [on-key add]))

;exercise 8

;A ColorPosn is (make-colorPosn Number Number Color)
;A ColorPosnList is one of:
; - empty
; - (cons (make-ColorPosn Number Number Color) ColorPosnList)
(define-struct ColorPosn (x y color))

;exercise 9

(define list4 empty)
(define list5 (cons (make-ColorPosn 5 23 "red") list4))
(define list6 (cons (make-ColorPosn 13 19 "yellow") list5))

;ColorPosnList -> image
;ColorDraw takes a ColorPosnList and draws all the ColorPosns
;on an empty-scene
;(draw list5) = (place-image (circle 5 "solid" "red") 5 23 (empty-scene 300 300))

(define (ColorDraw p)
  (cond [(empty? p) (empty-scene 300 300)]
        [else (place-image
               (circle 5 "solid" (ColorPosn-color (first p))) (ColorPosn-x (first p)) (ColorPosn-y (first p))
               (ColorDraw (rest p)))]))

(check-expect (ColorDraw list4) (empty-scene 300 300))
(check-expect (ColorDraw list5) (place-image (circle 5 "solid" "red") 5 23 (empty-scene 300 300)))

;ColorPosnList -> ColorPosnList
;ColorMove takes a ColorPosnList and makes another ColorPosnList
;where all the y coordinates increase by 1
;(move list5) = (cons (make-ColorPosn 5 24 "red") empty))

(define (ColorMove p)
  (cond [(empty? p) empty]
        [else (cons (make-ColorPosn(ColorPosn-x (first p)) (+ 1 (ColorPosn-y (first p))) (ColorPosn-color (first p))) (ColorMove (rest p)))]))

(check-expect (ColorMove list5) (cons (make-ColorPosn 5 24 "red") empty))
(check-expect (ColorMove list6) (cons (make-ColorPosn 13 20 "yellow") (cons (make-ColorPosn 5 24 "red") empty)))

;ColorPosnList KeyEvent -> ColorPosnList
;ColorAdd takes a ColorPosnList and adds a new ColorPosn
;with randomly made x and y coordinates
;(add list5) = (cons (make-ColorPosn (random 301) (random 301) (ColorPosn-color p)) list4)


(define (ColorAdd p ke)
  (cons (make-ColorPosn (random 301) (random 301) (make-color (random 256) (random 256) (random 256))) p))

;exercise 10

(define (ColorRun p)
  (big-bang p
            [on-tick ColorMove]
            [to-draw ColorDraw]
            [on-key ColorAdd]))
