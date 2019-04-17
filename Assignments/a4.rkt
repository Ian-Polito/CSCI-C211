;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;exercise 1

;draw String -> image
;draw takes a string and produces an image
;(draw "Hello World!") = (text "Hello World!" 24 "indigo")

(define (draw my-string)
  (overlay (text my-string 24 "indigo") (empty-scene 500 100)))

(check-expect (draw "hey")
              (overlay (text "hey" 24 "indigo") (empty-scene 500 100)))
(check-expect (draw "why tho")
              (overlay (text "why tho" 24 "indigo") (empty-scene 500 100)))

;exercise 2

(define init-string "Strength through chaos")

;typing key-input -> string
;typing takes a key-input and either prints
;the input as a string, or reverts the string to empty
;if a space was typed
;(typing "" "a") = ("a")
;(typing "" " ") = ("")

(define (typing w ke)
  (cond [(string=? ke " ") ""] [else (string-append  w  ke)]))

(check-expect (typing "" "a") "a")
(check-expect (typing "" " ") "")

(big-bang init-string
          [on-key typing]
          [to-draw draw])

;exercise 4

(define-struct date (year month day))
;Five functions created with this structure with their signatures

;(make-date) PositiveNumber String PositiveNumber -> date
;(date?) date -> boolean
;(date-year) date -> PositiveNumber
;(date-month) date -> String
;(date-day) date -> PositiveNumber

;exercise 5

(define-struct address (houseNumber street city state))
;houseNumber = PositiveNumber
;street = String
;city = String
;state = String

;exercise 6

;Template

;(define (process-address a)
  ;... (address-houseNumber a) ... (address-street a)
  ;... (address-city a) ... (address-state a))

;exercise 7

;streetSide address -> String
;streetSide takes an address and tells whether
;it is on the even or odd side of the street
;(streetSide (make-address 1020 "E Kirkwood Ave" "Bloomington" "IN"))
;= ("This is on the even side of the street.")
;(streetSide (make-address 107 "S Indiana Ave" "Bloomington" "IN"))
;= ("This is on the odd side of the street.")

(define (streetSide location)
  (cond [(even? (address-houseNumber location))
         "This is on the even side of the street."]
        [else "This is on the odd side of the street."]))

(check-expect (streetSide (make-address
                           1020 "E Kirkwood Ave" "Bloomington" "IN"))
              "This is on the even side of the street.")
(check-expect (streetSide (make-address
                           107 "S Indiana Ave" "Bloomington" "IN"))
              "This is on the odd side of the street.")

;exercise 8

;smallerStreet address address -> String
;smallerStreet takes two addresses and produces
;the one with the smaller street number
;(smallerStreet (make-address 1020 "E Kirkwood Ave"
; "Bloomington" "IN") (make-address 107 "S Indiana Ave"
; "Bloomington" "IN"))
; = "107 S Indiana Ave Bloomington IN"

(define (smallerStreet loc1 loc2)
  (cond [(< (address-houseNumber loc1)
            (address-houseNumber loc2))
         (string-append (number->string
                         (address-houseNumber loc1))
                        " " (address-street loc1) " "
                        (address-city loc1) " "
                        (address-state loc1))]
        [else (string-append (number->string
                              (address-houseNumber loc2))
                             " " (address-street loc2) " "
                             (address-city loc2) " "
                             (address-state loc2))]))

(check-expect (smallerStreet (make-address
                              1020 "E Kirkwood Ave" "Bloomington" "IN")
                             (make-address
                              107 "S Indiana Ave" "Bloomington" "IN"))
              "107 S Indiana Ave Bloomington IN")
(check-expect (smallerStreet
               (make-address 308 "College Dr" "Bloomington" "IN")
                             (make-address 709 "College Dr"
                                           "Bloomington" "IN"))
              "308 College Dr Bloomington IN")

;exercise 9

;formatStreet address -> String
;formatStreet takes an address and formats it so
;it could be written on a letter
;(formatStreet (make-address 18455 "S Figueroa St"
;"Gardena" "CA")) = "18455 S Figueroa St, Gardena CA"

(define (formatStreet loc)
  (string-append (number->string (address-houseNumber loc))
                 " " (address-street loc) ", "
                 (address-city loc) " " (address-state loc)))

(check-expect (formatStreet
               (make-address 18455 "S Figueroa St" "Gardena" "CA"))
              "18455 S Figueroa St, Gardena CA")

;exercise 83

(define-struct editor (pre post))

;render editor -> image
;render takes an editor and produces an image
;(render (make-editor "Hello " "World!")) = (overlay/align "left" "center"
               ;(text "Hello World!" 11 "black")
               ;(empty-scene 200 20)))

(define CURSOR (rectangle 1 20 "solid" "red"))

(define (render edit)
  (overlay/align "left" "center"
                 (beside (text (editor-pre edit) 11 "black")
                         CURSOR (text (editor-post edit) 11 "black"))
                 (empty-scene 200 20)))

(check-expect (render (make-editor "Hello " "World!"))
              (overlay/align "left" "center"
                             (beside (text "Hello " 11 "black")
                                     CURSOR (text "World!" 11 "black"))
                             (empty-scene 200 20)))

;exercise 84

;edit editor keyevent -> editor
;edit takes an editor and a key event and adds the single-character
;keyevent to the end of the pre field of the given editor, unless the
;keyevent is the backspace character, in which it deletes the last character
;in the pre field of the given editor. It also can take the 'left' and 'right'
;keyevents and move the cursor accordingly. Any other keys are ignored.
;(edit (make-editor "Hello " "World!") "c") = (make-editor "Hello c" "World!")
;(edit (make-editor "Hello " "World!") "\b") = (make-editor "Hello" "World!")
;(edit (make-editor "Hello " "World!") "left") = (make-editor "Hello" "World!")
;(edit (make-editor "Hello " "World!")
; "right") = (make-editor "Hello" "World!")

(define (edit ed ke)
  (cond [(or (and (string<=? "a" ke)
                  (string>=? "z" ke) (= (string-length ke) 1))
             (and (string<=? "A" ke)
                  (string>=? "Z" ke) (= (string-length ke) 1))
             (string=? " " ke))
         (make-editor (string-append (editor-pre ed) ke) (editor-post ed))]
        [(and (string=? ke "\b") (> (string-length (editor-pre ed)) 0))
         (make-editor (substring (editor-pre ed) 0 (- (string-length
                                                       (editor-pre ed)) 1))
                      (editor-post ed))]
        [(and (string=? ke "left") (> (string-length (editor-pre ed)) 0))
         (make-editor (substring (editor-pre ed) 0
                                 (- (string-length (editor-pre ed)) 1))
                      (string-append (substring (editor-pre ed)
                                                (- (string-length
                                                    (editor-pre ed)) 1)
                                                (string-length
                                                 (editor-pre ed)))
                                     (editor-post ed)))]
        [(and (string=? ke "right") (> (string-length (editor-post ed)) 0))
         (make-editor (string-append (editor-pre ed) (substring
                                                      (editor-post ed) 0 1))
                      (substring (editor-post ed) 1 (string-length
                                                     (editor-post ed))))]
        [else ed]))

(check-expect (edit (make-editor "Hello " "World!") "c")
              (make-editor "Hello c" "World!"))
(check-expect (edit (make-editor "Hello " "World!") "a")
              (make-editor "Hello a" "World!"))
(check-expect (edit (make-editor "Hello " "World!") "z")
              (make-editor "Hello z" "World!"))
(check-expect (edit (make-editor "Hello " "World!") "H")
              (make-editor "Hello H" "World!"))
(check-expect (edit (make-editor "Hello " "World!") "A")
              (make-editor "Hello A" "World!"))
(check-expect (edit (make-editor "Hello " "World!") "Z")
              (make-editor "Hello Z" "World!"))
(check-expect (edit (make-editor "Hello " "World!") " ")
              (make-editor "Hello  " "World!"))
(check-expect (edit (make-editor "Hello " "World!") "\b")
              (make-editor "Hello" "World!"))
(check-expect (edit (make-editor "" "World!") "\b")
              (make-editor "" "World!"))
(check-expect (edit (make-editor "Hello " "World!") "\t")
              (make-editor "Hello " "World!"))
(check-expect (edit (make-editor "Hello " "World!") "left")
              (make-editor "Hello" " World!"))
(check-expect (edit (make-editor "Hello " "World!") "right")
              (make-editor "Hello W" "orld!"))
(check-expect (edit (make-editor "" "World!") "left")
              (make-editor "" "World!"))
(check-expect (edit (make-editor "Hello " "") "right")
              (make-editor "Hello " ""))

;exercise 85

;run string -> big bang
;run takes a string and creates an editor, where the string
;is the pre field, and launches
;a big bang using the previous two functions for the to-draw
;and on-key clauses

;(define (run st)
  ;(big-bang (make-editor st "")
            ;[on-key edit]
            ;[to-draw render]))

;I had to comment out 'run' because of a handin server error
;that complained about "submit error: Error in your code --
;#(struct:object:text% ...):243:2: begin (possibly implicit):
;empty form is not allowed in: (begin)".
;It worked fine before I commented it out when I ran it in
;my own DrRacket

