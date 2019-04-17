;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define init-speed 1)
(define init-angle (/ pi 4))

(define init-x-vel (* init-speed (cos init-angle)))
(define init-y-vel (* init-speed (sin init-angle)))

(define (y-pos t) (+ (* init-y-vel t) (* -.002 0.5 (* t t) )))
(define (x-pos t) (* init-x-vel t))

(define (draw-sprite x y) (place-image (circle 10 "solid" "red") x (- 200 y) (empty-scene 500 200)))

(define (launch t) (draw-sprite (x-pos t) (y-pos t)))

(animate launch)