;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An Upload is (list Number Number)
; where the two numbers are x and y coordinates
 
; A Download is [ListOf Info]
; An Info is (list String Upload)
; where the string is the name of a world

(define BACKGROUND (empty-scene 640 480))

;exercise 1

(define upload1 (list 50 100 "red"))
(define upload2 (list 200 300))

(define download1 (list (list "Spoderman says hi" upload1)))
(define download2 (list (list "Coordinate 1" upload1)
                        (list "Coordinate 2" upload2)))

;exercise 2

;draw-download : Download -> Image
;draw-download takes a download and draws each info in it as text
;at the location specified in the upload on BACKGROUND
(define (draw-download d)
  (cond [(empty? d) BACKGROUND]
        [else (place-image (text (first (first d)) 12 "indigo")
                           (first (first (rest (first d))))
                           (first (rest (first (rest (first d)))))
                           (draw-download (rest d)))]))

;exercise 3

;A World is Download

;(big-bang download1
          ;[to-draw draw-download])

;exercise 4

; A Universe is [ListOf Entry]
; An Entry is (make-entry IWorld Upload)
(define-struct entry [world upload])
 
; For example:
(define universe0 empty)
(define universe1
  (list (make-entry iworld1 (list 10 20))
        (make-entry iworld2 (list 30 40))))

;update : Universe IWorld Upload -> Universe
;update takes the inputs and either adds a new Entry to the given Universe
;or replaces an existing Entry
(define (update uni world up)
  (cond [(empty?) (make-entry world up)]
        [(iworld=? world
                   (entry-world (first uni)))
         (cons (make-entry world up) (rest uni))]
        [else (cons (first uni) (update (rest uni) world up))]))

;exercise 5

;universe->download : Universe -> Download
;converts the given universe to a download so it can be drawn
(define (universe->download uni)
  (cond [(empty? uni) empty]
        [else (cons (cons (iworld-name (entry-world (first uni)))
                          (list (entry-upload (first uni))))
                    (universe->download (rest uni)))]))
(check-expect (universe->download universe0) empty)
(check-expect (universe->download universe1)
              (list (list "iworld1" (list 10 20))
                    (list "iworld2" (list 30 40))))

;exercise 6

;A Mail is (make-mail IWorld Download)

;broadcast : Universe -> [ListOf Mail]
;broadcast takes a universe and returns a LoM that broadcasts all the worlds' locations
;to all the worlds
(define (broadcast uni)
  (cond [(empty? uni) empty]
        [else (cons (make-mail (entry-world (first uni)) (universe->download uni)) (broadcast-help (rest uni) uni))]))
(check-expect (broadcast universe0) empty)
(check-expect (broadcast universe1)
              (list (make-mail iworld1 (universe->download universe1))
                    (make-mail iworld2 (universe->download universe1))))

(define (broadcast-help uni uni2)
  (cond [(empty? uni) empty]
        [else (cons (make-mail (entry-world (first uni)) (universe->download uni2)) (broadcast-help (rest uni) uni2))]))

;exercise 7

;A Bundle is (make-bundle Universe [ListOf Mail] [ListOf IWorld])

; msg : Universe IWorld Upload -> Bundle
; Receive a new location from a world and broadcast everything to everyone
(define (msg uni world up)
  5)
(check-expect
 (msg universe0 iworld3 (list 50 60))
 (make-bundle (list (make-entry iworld3 (list 50 60)))
              (list (make-mail iworld3 (list (list "iworld3" (list 50 60)))))
              empty))

; new : Universe IWorld -> Universe
; Do nothing when a new world connects
(define (new u iw) u)

;exercise 8

; mouse : World Number Number MouseEvent -> WorldResult
; Send the location to the universe when the mouse is clicked
(define (mouse w num1 num2 Me)
  (cond [(string=? Me "button-down") (make-package empty (list num1 num2))]
        [else w]))
(check-expect (mouse empty 50 60 "move") empty)
(check-expect (mouse empty 50 60 "button-down")
              (make-package empty (list 50 60)))

;(big-bang empty
          ;[to-draw draw-download]
          ;[on-mouse mouse]
          ;[on-receive (lambda (w download) download)]
          ;[name "Spoderman"]
          ;[register "156.56.167.67"]
          ;[port 5000])

;exercise 9

