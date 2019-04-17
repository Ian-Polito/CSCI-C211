;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;exercise 1

(define (process-dessert d)
  (... (cond
         [(cupcake? d) ...]
         [(pie? d) ...]) ...))


(define-struct cupcake (frosting))
(define-struct pie (filling slices))

;exercise 2

(define cupcake1 (make-cupcake "chocolate"))
(define pie1 (make-pie "vanilla" 2))
(define cupcake2 (make-cupcake "vanilla"))

;exercise 3

;desert -> positiveNumber
;calculateCalories takes a dessert and calculates the
;calories that are in it.
;(calculateCalories cupcake1) = 150
;(calculateCalories pie1) = 350

(define (calculateCalories des)
  (cond [(cupcake? des) (cupcakeCalories des)]
        [else (pieCalories des)]))

(check-expect (calculateCalories cupcake1) 150)
(check-expect (calculateCalories pie1) 350)

;cupcake -> positiveNumber
;cupcakeCalories takes a cupcake and calculates the
;calories that are in it.
;(cupcakeCalories cupcake2) = 125

(define (cupcakeCalories des)
  (cond [(string=? (cupcake-frosting des) "chocolate") 150]
        [else 125]))

(check-expect (cupcakeCalories cupcake1) 150)
(check-expect (cupcakeCalories cupcake2) 125)

;pie -> positiveNumber
;pieCalories takes a pie and calculates the
;calories that are in it.
;(pieCalories pie1) = 350

(define (pieCalories des)
  (* (pie-slices des) 175))

(check-expect (pieCalories pie1) 350)

;exercise 4

;An emptyCase is (make-emptyCase)
(define-struct emptyCase ())

;A dessertCase is (make-dessertCase dessert severalCases)
;A severalCases is one of:
; - emptyCase
; - dessertCase
(define-struct dessertCase (des case))

;exercise 5

(define case1 (make-emptyCase))
(define case2 (make-dessertCase cupcake1 case1))
(define case3 (make-dessertCase cupcake2 case2))

;exercise 6

(define (process-cases case)
  (... (dessertCase-des case) ...
       (process-cases (multipleCases-case case)) ...))

;exercise 7

;case -> positiveNumber
;total-calories takes a case of desserts and calculates the total calories of all desserts in the case
;(total-calories case3) = 275

(define (total-calories c)
  (cond [(emptyCase? c) 0]
        [else (+ (calculateCalories (dessertCase-des c)) (total-calories (dessertCase-case c)))]))

(check-expect (total-calories case3) 275)

;exercise 8

(define-struct no-cords ())
(define-struct many-cords (c cord))

(define (draw-cords cord)
  (cond [(no-cords? cord) (draw-no-cords cord)]
        [else (draw-many-cords cord)]))
(define (draw-no-cords cord)
  (empty-scene 500 500))
(define (draw-many-cords cord)
  (cond [(no-cords? cord) (draw-no-cords cord)]
        [else (place-image (circle 5 "solid" "black")
                            (posn-x (many-cords-c cord))
                            (posn-y (many-cords-c cord))
                            (draw-many-cords (many-cords-cord cord)))]))

(define (mouse sps x y event)
  (cond [(or (string=? event "button-down") (string=? event "drag")) (make-many-cords (make-posn x y) sps)]
        [else sps]))

(big-bang  (make-no-cords)
           [to-draw draw-many-cords]
           [on-mouse mouse])

