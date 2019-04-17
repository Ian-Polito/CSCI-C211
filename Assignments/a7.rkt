;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

;exercise 1

;a frequency is
; - (make-frequency String Number)
(define-struct frequency [str num])

;exercise 2

;a ListOfString is one of
; - empty
; - (cons String ListOfString)

(define strlist1 (cons "hey" empty))
(define strlist2 (cons "there" strlist1))
(define strlist3 (cons "hello" strlist2))

;a ListOfFrequency is one of
; - empty
; - (cons frequency ListOfFrequency)

(define list1 (cons (make-frequency "hey" 1) empty))
(define list2 (cons (make-frequency "there" 1) list1))

;exercise 3

;ListOfFrequency String -> ListOfFrequency
;count-word takes a ListOfFrequency and a string and
;searches for a frequency in the list with a string
;that matches the given string and adds one to the num
;of that frequency, otherwise adds a new frequency to the list

(define (count-word list str)
  (cond [(contains? list str) (count-word-helper list str)]
        [else (cons (make-frequency str 1) list)]))

(check-expect (count-word empty "hey") list1)
(check-expect (count-word list1 "hey") (cons (make-frequency "hey" 2) empty))
(check-expect (count-word list1 "there") list2)

;ListOfFrequency String -> boolean
;contains? takes a list and a string and returns true
;if the string is found in any of the frequencys in
;the list, otherwise returns false

(define (contains? list str)
  (cond [(empty? list) false]
        [else (or (string=? str (frequency-str (first list))) (contains? (rest list) str))]))

(check-expect (contains? list1 "hey") true)
(check-expect (contains? list2 "there") true)
(check-expect (contains? list2 "yo") false)

;ListOfFrequency String -> ListOfFrequency
;count-word-helper takes a ListOfFrequency and a string and
;searches for a frequency in the list with a string
;that matches the given string and adds one to the num
;of that frequency

(define (count-word-helper list str)
  (cond [(empty? list) empty]
        [(string=? (frequency-str (first list)) str) (cons (make-frequency str (+ 1 (frequency-num (first list)))) (count-word-helper (rest list) str))]
        [else (cons (first list) (count-word-helper (rest list) str))]))

(check-expect (count-word-helper list2 "there") (cons (make-frequency "there" 2) list1))

;exercise 4

;ListOfString -> ListOfFrequency
;count-all-words takes a ListOfString and produces a
;ListOfFrequency with all the frequencies counted from
;the list

(define (count-all-words list)
  (cond [(empty? list) empty]
        [else (count-all-words-helper empty list)]))

(check-expect (count-all-words empty) empty)
(check-expect (count-all-words strlist1) list1)
(check-expect (count-all-words strlist2) (cons (make-frequency "hey" 1) (cons (make-frequency "there" 1) empty)))

;ListOfFrequency ListOfString -> ListOfFrequency
;count-all-words-helper takes a ListOfFrequency and
;a ListOfString and adds all the words to it

(define (count-all-words-helper LoF LoS)
  (cond [(empty? LoS) LoF]
        [else (count-all-words-helper (count-word LoF (first LoS)) (rest LoS))]))

;exercise 5

(define hamlet-txt (read-words "hamlet.txt"))

;(define hamlet-freq (count-all-words hamlet-txt))

;exercise 6

(define list3 (cons (make-frequency "there" 101) (cons (make-frequency "goodbye" 200) list1)))

;ListOfFrequency -> ListOfFrequency
;frequents takes a ListOfFrequency and creates a
;new one with only the words from the first list
;that occured more than 100 times.

(define (frequents list)
  (cond [(empty? list) empty]
        [(< 100 (frequency-num (first list))) (cons (make-frequency (frequency-str (first list)) (frequency-num (first list))) (frequents (rest list)))]
        [else (frequents (rest list))]))

(check-expect (frequents list3) (cons (make-frequency "there" 101) (cons (make-frequency "goodbye" 200) empty)))
;(define hamlet-freq-hundred (frequents hamlet-txt))

;exercise 7

; A Mobile is one of:
; - (make-leaf Number)
; - (make-rod Mobile Number Number Mobile)
(define-struct leaf [weight])
(define-struct rod [lm ld rd rm])

(define leaf1 (make-leaf 100))
(define leaf2 (make-leaf 42))
(define leaf3 (make-leaf 19))
(define mobile1 (make-rod leaf1 25 18 leaf2))
(define mobile2 (make-rod mobile1 32 7 leaf3))
(define leaf4 (make-leaf 10))
(define mobile3 (make-rod leaf4 10 10 leaf4))
(define mobile4 (make-rod mobile3 10 10 mobile3))

;Mobile -> Number
;weight takes a mobile and determines the weight of
;all of the leaves

(define (weight m)
  (cond [(leaf? m) (leaf-weight m)]
        [else (+ (weight (rod-lm m)) (weight (rod-rm m)))]))

(check-expect (weight leaf3) 19)
(check-expect (weight mobile1) 142)
(check-expect (weight mobile2) 161)

;exercise 8

;mobile -> Number
;average-leaf-weight takes a mobile and gives
;an average of all the leaf weights

(define (average-leaf-weight m)
  (cond [(leaf? m) (leaf-weight m)]
        [else (/ (weight m) (number-leaves m))]))

(check-expect (average-leaf-weight leaf2) 42)
(check-expect (average-leaf-weight mobile1) 71)
(check-expect (average-leaf-weight mobile2) (/ 161 3))

;mobile -> number
;number-leaves takes a mobile and returns the
;number of leaves it contains

(define (number-leaves m)
  (cond [(leaf? m) 1]
        [else (+ (number-leaves (rod-lm m)) (number-leaves (rod-rm m)))]))

(check-expect (number-leaves mobile1) 2)
(check-expect (number-leaves mobile2) 3)

;exercise 9

;Mobile -> boolean
;all-balanced? takes a mobile and returns a boolean
;based on if the mobile is balanced everywhere


(define (all-balanced? m)
  (cond [(leaf? m) true]
        [(and (leaf? (rod-lm m)) (leaf? (rod-rm m))) (balanced? m)]
        [else (and (balanced? m) (all-balanced? (rod-lm m)) (all-balanced? (rod-rm m)))]))

(check-expect (all-balanced? leaf1) true)
(check-expect (all-balanced? mobile1) false)
(check-expect (all-balanced? mobile2) false)
(check-expect (all-balanced? mobile4) true)

; balanced? : Mobile -> Boolean
; determines whether the given mobile is balanced at the top

(define (balanced? m)
  (cond [(leaf? m) true]
        [(rod? m) (= (* (rod-ld m) (weight (rod-lm m)))
                     (* (rod-rd m) (weight (rod-rm m))))]))

(check-expect (balanced? leaf1) true)
(check-expect (balanced? mobile1) false)

;exercise 10

;mobile -> mobile
;lighten takes a mobile and halves the weight of
;all leaves on it

(define (lighten m)
  (cond [(leaf? m) (make-leaf (/ (leaf-weight m) 2))]
        [else (make-rod (lighten (rod-lm m)) (rod-ld m) (rod-rd m) (lighten (rod-rm m)))]))

(check-expect (lighten mobile1) (make-rod (make-leaf 50) 25 18 (make-leaf 21)))

;exercise 11

;mobile Number -> mobile
;enlarge takes a mobile and a number and
;multiplies all the distances by the given num

(define (enlarge m num)
  (cond [(leaf? m) m]
        [else (make-rod (enlarge (rod-lm m) num) (* num (rod-ld m)) (* num (rod-rd m)) (enlarge (rod-rm m) num))]))

(check-expect (enlarge mobile3 3) (make-rod leaf4 30 30 leaf4))

;exercise 12

;Number -> mobile
;all-balanced-mobile takes a positive number and returns a
;mobile with that many leaves that is completely balanced

(define (all-balanced-mobile num)
  (cond [(even? num) (cond [(= num 2) (make-rod (make-leaf 10) 10 10 (make-leaf 10))]
                           [else (make-rod (all-balanced-mobile (- num 2)) 10 10 (all-balanced-mobile (- num 2)))])]
        [else (make-rod (all-balanced-mobile (- num 1)) 10 10 (make-leaf (* (- num 1) 10)))]))

(check-expect (all-balanced-mobile 2) mobile3)
(check-expect (all-balanced-mobile 4) mobile4)
(check-expect (all-balanced-mobile 3) (make-rod mobile3 10 10 (make-leaf 20)))
