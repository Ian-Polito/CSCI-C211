;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

;exercise 1

;a ListOfString is
; - (cons String empty)

(define los1 empty)
(define los2 (cons "hey" empty))
(define los3 (cons "there" (cons "hey" empty)))

#;
(define (process-los los)
  (cond [(empty? los) ...]
        [else ... (first los) ... (process-los (rest los))]))

;exercise 2

;ListOfString String -> boolean
;has-word? takes a listofstring and a string
;and returns true if the string is in the given
;listofstrings, otherwise returns false
;(has-word? (make-ListOfString "hey" (make-emptylos)) "hey") = true

(define (has-word? los str)
  (cond [(empty? los) false]
        [else (or (string=? str (first los)) (has-word? (rest los) str))]))

(check-expect (has-word? los1 "any") false)
(check-expect (has-word? los2 "hey") true)
(check-expect (has-word? los3 "there") true)
(check-expect (has-word? los3 "naught") false)

;exercise 3

(define file-names
  (cons "thefly.txt"
        (cons "thegerm.txt"
              (cons "theoctopus.txt"
                    (cons "theostrich.txt"
                          (cons "thetermite.txt"
                                empty))))))

;exercise 4

;String String -> boolean
;file-has-word? takes a filename and a string
;and returns true if the string is in the given
;file, otherwise returns false
;(file-has-word? "thefly.txt" "fly") = true

(define (file-has-word? file str)
  (has-word? (read-words file) str))

(check-expect (file-has-word? "thefly.txt" "fly,") true)
(check-expect (file-has-word? "thetermite.txt" "Cousin") true)
(check-expect (file-has-word? "thegerm.txt" "pragmatic") false)

;exercise 5

;ListOfString String -> ListOfString
;search-files takes a list of file names and
;searches the contents of each file for the given
;string. Returns a ListOfString containing the
;filenames of files that contain the string.
;(search-files file-names "fly,") = (cons "thefly.txt" empty)

(define (search-files los str)
  (cond [(empty? los) empty]
        [(file-has-word? (first los) str) (cons (first los) (search-files (rest los) str))]
        [else (search-files (rest los) str)]))

(check-expect (search-files file-names "fly,") (cons "thefly.txt" empty))
(check-expect (search-files file-names "is") (cons "thegerm.txt"
                                                   (cons "theoctopus.txt"
                                                         (cons "theostrich.txt"
                                                               (cons "thetermite.txt" empty)))))

;exercise 6

(define exercise6 (search-files file-names "it"))

;exercise 7

;a ripple is
; - (make-ripple Number Number Number)
(define-struct ripple (x y size))

;a ListOfRipples is
; - (cons ripple empty)
(define list1 empty)
(define list2 (cons (make-ripple 5 23 1) list1))
(define list3 (cons (make-ripple 13 19 5) list2))

;exercise 8

#;
(define (process-ripples p)
  (cond [(empty? p) ...]
        [else ... (first p) ... (process-ripples (rest p))]))

;exercise 9

;ListOfRipples Number Number MouseEvent
;RippleMouse takes a MouseEvent and the x and y coordinates
;of the mouse and creates a new ripple using this data and
;adds it to the given ListOfRipples
;(RippleMouse list1 70 82 "button-down") = ...

(define (RippleMouse p x y event)
  (cond [(string=? event "button-down") (cons (make-ripple x y 1) p)]
        [else p]))

(check-expect (RippleMouse list1 70 82 "button-down") (cons (make-ripple 70 82 1) empty))

;ListOfRipples -> ListOfRipples
;RippleGrow takes a ListOfRipples and increases the size of all ripples contained in the list
;(RippleGrow list1) = ...

(define (RippleGrow p)
  (cond [(empty? p) empty]
        [else (cons (make-ripple (ripple-x (first p)) (ripple-y (first p)) (+ 1 (ripple-size (first p)))) (RippleGrow (rest p)))]))

(check-expect (RippleGrow list2) (cons (make-ripple 5 23 2) list1))

;ListOfRipples -> image
;DrawRipples takes a ListOfRipples and draws it
;(DrawRipples list1) = ...

(define (DrawRipples p)
  (cond [(empty? p) (empty-scene 300 300)]
        [else (place-image
               (circle (ripple-size (first p)) "outline" "blue") (ripple-x (first p)) (ripple-y (first p))
               (DrawRipples (rest p)))]))

(check-expect (DrawRipples list1) (empty-scene 300 300))
(check-expect (DrawRipples list2) (place-image (circle 1 "outline" "blue") 5 23 (empty-scene 300 300)))

(big-bang list1
          [on-tick RippleGrow]
          [to-draw DrawRipples]
          [on-mouse RippleMouse])
