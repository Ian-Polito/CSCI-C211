;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;exercise 1

;A Pair is (make-pair Number Number)
(define-struct pair (x y))

;exercise 2

;A Pairs is one of
; - (make-no-pairs)
; - (make-some-pairs)
(define-struct no-pairs ())
(define-struct some-pairs (pair pairs))

;exercise 3

#;
(define (process-pair p)
  ... (pair-x p) ... (pair-y p))

#;
(define (process-pairs p)
  (cond [(no-pairs? p) ...]
        [else (process-pairs (some-pairs-pairs p))]))

;exercise 4

(define pair1 (make-pair 5 15))
(define pair2 (make-pair 25 80))

(define test1 (make-no-pairs))
(define test2 (make-some-pairs pair1 test1))
(define test3 (make-some-pairs pair2 test2))

;pairs -> image
;draw-pairs takes a pairs and draws the pairs it contains as circles
;based on their x and y coordinates

(define (draw-pairs p)
  (cond [(no-pairs? p) (draw-no-pairs p)]
        [else (draw-many-pairs p)]))

;no-pairs -> image
;draw-no-pairs draws an empty scene

(define (draw-no-pairs p)
  (empty-scene 500 500))

;pairs -> image
;draw-many-pairs takes a pairs and draws the pairs it contains as circles
;based on their x and y coordinates

(define (draw-many-pairs p)
  (cond [(no-pairs? p) (draw-no-pairs p)]
        [else (place-image (circle 10 "solid" "black")
                            (pair-x (some-pairs-pair p))
                            (pair-y (some-pairs-pair p))
                            (draw-many-pairs (some-pairs-pairs p)))]))

(check-expect (draw-pairs test1) (empty-scene 500 500))
(check-expect (draw-pairs test2) (place-image (circle 10 "solid" "black") 5 15 (empty-scene 500 500)))

;exercise 5

;pairs Number Number MouseEvent -> pairs
;any-paint takes a pairs and two numbers for x and y coordinates
;and adds a new pair to it

(define (any-paint p x y w)
  (make-some-pairs (make-pair x y) p))

(check-expect (any-paint test2 25 80 "button_down") test3)

;pairs KeyEvent -> pairs
;any-undo takes a pairs and a key event and removes the most
;recently added pair

(define (any-undo p ke)
  (cond [(some-pairs? p) (some-pairs-pairs p)]
        [else p]))

(check-expect (any-undo test3 "k") test2)
(check-expect (any-undo test1 "k") test1)

(define (run-any p)
  (big-bang p
            [on-mouse any-paint]
            [on-key any-undo]
            [to-draw draw-pairs]))

;exercise 6

;pairs Number Number MouseEvent -> pairs
;paint takes a pairs and two numbers for x and y coordinates
;and adds a new pair to it if the MouseEvent is "button-down"
;or "drag"

(define (paint p x y w)
  (cond [(or (string=? w "drag") (string=? w "button-down")) (make-some-pairs (make-pair x y) p)]
        [else p]))

(check-expect (paint test2 25 80 "button-down") test3)
(check-expect (paint test2 25 80 "move") test2)

;pairs KeyEvent -> pairs
;undo takes a pairs and a key event and removes the most
;recently added pair if the KeyEvent is "z"

(define (undo p ke)
  (cond [(string=? ke "z")
         (cond [(some-pairs? p) (some-pairs-pairs p)]
               [else p])]
        [else p]))

(check-expect (undo test3 "k") test3)
(check-expect (undo test3 "z") test2)
(check-expect (undo test1 "z") test1)

(define (run p)
  (big-bang p
            [on-mouse paint]
            [on-key undo]
            [to-draw draw-pairs]))

;exercise 7

; A SolarObject is one of:
; - (make-sun)
; - (make-planet Number SolarObject)
 
(define-struct sun ())
(define-struct planet (dist inner))

(define sun1 (make-sun))
(define sys1 (make-planet 138 sun1))
(define sys2 (make-planet 843 sys1))
(define sys3 (make-planet 291 sys2))

;exercise 8

#;
(define (process-solar-object p)
  (cond [(sun? p) ...]
        [else (process-solar-object (planet-inner p))]))

;exercise 9

;SolarObject -> Number
;distance-of-solar-object takes a SolarObject and calculates
;the distance from the outermost planet to the center of
;the solar system

(define (distance-of-solar-object p)
  (cond [(sun? p) 0]
        [else (+ (planet-dist p) (distance-of-solar-object (planet-inner p)))]))

(check-expect (distance-of-solar-object sun1) 0)
(check-expect (distance-of-solar-object sys1) 138)
(check-expect (distance-of-solar-object sys3) 1272)

;exercise 10

;SolarObject Number -> SolarObject
;add-to-solar-object takes a SolarObject and a distance
;and creates a new SolarObject with a new planet, created
;using the given distance

(define (add-to-solar-object p dist)
  (make-planet dist p))

(check-expect (add-to-solar-object sys2 291) sys3)

;exercise 11

;SolarObject -> image
;draw-solar-object takes a SolarObject and draws it

(define (draw-solar-object p)
  (cond [(sun? p) (circle 20 "solid" "yellow")]
        [else (overlay
               (circle (/ (distance-of-solar-object p) 2) "outline" "black")
               (draw-solar-object (planet-inner p)))]))

(check-expect (draw-solar-object sys3) (overlay
                                        (circle (/ 1272 2) "outline" "black")
                                        (circle (/ 981 2) "outline" "black")
                                        (circle 69 "outline" "black")
                                        (circle 20 "solid" "yellow")))
