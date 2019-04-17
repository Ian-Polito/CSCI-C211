;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;exercise 1

;tilted-star num -> image
;tilted-star takes a number and produces a tilted star image with color based on whether num is even or odd
;(tilted-star 68) = (rotate deg (star 100 "solid" "goldenrod"))
;(tilted-star 67) = (rotate deg (star 100 "solid" "white"))

(define (tilted-star deg) (cond [(= (remainder deg 2) 0) (rotate deg (star 100 "solid" "goldenrod"))] [else (rotate deg (star 100 "solid" "white"))]))

(define (flickering-tilted-star deg) (rotate deg (star 100 "solid" (cond [(even? deg) "goldenrod"] [else "white"]))))

(check-expect (tilted-star 68) (rotate 68 (star 100 "solid" "goldenrod")))
(check-expect (tilted-star 67) (rotate 67 (star 100 "solid" "white")))

;exercise 2

;decibels num -> string
;decibels take a number and return something that could cause that much noise
;(decibels 67) = "Television."

(define (decibels db) (cond [(<= db 39) "Whispering."]
                            [(and (>= db 60) (<= db 69)) "Television."]
                            [(and (>= db 98) (<= db 99)) "Honda S2000 Factory Exhaust."]
                            [(and (>= db 101) (<= db 105)) "Ferrari 458 Factory Exhaust."]
                            [(and (>= db 145) (<= db 149)) "McDonnell Douglas F-15C at takeoff."]
                            [(and (>= db 150) (<= db 154)) "Lockheed Martin F-35B at hover."]
                            [(and (>= db 280) (<= db 300)) "Thermonuclear warhead."]
                            [(> db 1000) "Disaster area, heard from a concrete bunker 37 mi away."]))

(check-expect (decibels 67) "Television.")
(check-expect (decibels 290) "Thermonuclear warhead.")

;exercise 3

;draw-shape string, string, number -> image
;draw-shape takes a string, a string, and a number and produces an image based on the parameters given
;(draw-shape "circle" "red" 50) = (circle 25 "solid" "red")

(define (draw-shape shape color width) (cond
                                         [(string=? shape "circle") (circle (/ width 2) "solid" color)]
                                         [(string=? shape "box") (square (/ width 2) "solid" color)]
                                         [(string=? shape "triangle") (triangle (/ width 2) "solid" color)]))

(check-expect (draw-shape "circle" "red" 50) (circle 25 "solid" "red"))
(check-expect (draw-shape "box" "blue" 100) (square 50 "solid" "blue"))
(check-expect (draw-shape "triangle" "yellow" 80) (triangle 40 "solid" "yellow"))

;exercise 4

;red-frame image -> image
;red-frame takes an image and draws a red frame around it
;(red-frame (circle 50 "solid" "blue") = (overlay (circle 50 "solid" "blue")
                                        ;(rectangle 101 101 "solid" "white")
                                        ;(rectangle 104 104 "solid" "red")))

(define (get-width i) (image-width i))
(define (red-frame image) (overlay image (rectangle (+ (get-width image) 1) (+ (get-width image) 1) "solid" "white") (rectangle (+ (get-width image) 4) (+ (get-width image) 4) "solid" "red")))

(check-expect (red-frame (circle 50 "solid" "blue"))
              (overlay (circle 50 "solid" "blue")
                       (rectangle 101 101 'solid 'white)
                       (rectangle 104 104 'solid 'red)))

(check-expect (red-frame (rectangle 77 77 "solid" "yellow"))
              (overlay (rectangle 77 77 "solid" "yellow")
                       (rectangle 78 78 'solid 'white)
                       (rectangle 81 81 'solid 'red)))

(check-expect (red-frame (triangle 24 "solid" "green"))
              (overlay (triangle 24 "solid" "green")
                       (rectangle 25 25 'solid 'white)
                       (rectangle 28 28 'solid 'red)))
