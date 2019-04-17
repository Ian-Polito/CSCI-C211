;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

;exercise 1

;farthest : Posn ListOfPosn -> Posn
;farthest looks through the LoP and finds the Posn that
;is farthest from the given Posn
(define (farthest p lop)
  (local [(define (retrieve lop p num) (cond [(empty? lop) empty]
                                            [(= (dist p (first lop)) num) (first lop)]
                                            [else (retrieve (rest lop) p num)]))
           (define (dist p0 p1)
             (sqrt (+ (expt (- (posn-x p0) (posn-x p1)) 2)
                      (expt (- (posn-y p0) (posn-y p1)) 2))))]
    (retrieve lop p (foldl (lambda (cur ans) (cond [(< ans (dist cur p)) (dist cur p)]
                                                     [else ans])) 0 lop))))
(check-expect (farthest (make-posn 0 0) (list (make-posn 0 0)
                                                (make-posn 5 5)
                                                (make-posn 9 9)
                                                (make-posn 20 20)))
                        (make-posn 20 20))

;exercise 2

;count : ListOfNumbers -> Number
;count takes a LoN and adds them up
(define (count lon)
  (foldl + 0 lon))
(check-expect (count (list 3 2 1 1 2)) 9)
(check-expect (count empty) 0)

;exercise 3

;sometimes-hello : ListOfString -> ListOfString
;sometimes-hello takes a list of names and adds "Hello, "
;before some of the names randomly
(define (sometimes-hello los)
  (map (lambda (ans) (cond [(= (random 2) 0) (string-append "Hello, " ans)]
                           [else ans])) los))

;exercise 4

;evens-first : Nat -> ListOfNumbers
;evens-first constructs a list with a given number of elements
;that are all even starting with 0
(define (evens-first n)
  (build-list n (lambda (x) (* 2 x))))
(check-expect (evens-first 4) (list 0 2 4 6))

;evens*-first : Nat -> ListOfNumbers
;evens*-first constructs a list with a given number of elements
;that are all even starting with 2
(define (evens*-first n)
  (build-list n (lambda (x) (+ 2 (* 2 x)))))
(check-expect (evens*-first 4) (list 2 4 6 8))

;odds-first : Nat -> ListOfNumbers
;odds-first constructs a list with a given number of elements
;that are all odd starting with 1
(define (odds-first n)
  (build-list n (lambda (x) (+ 1 (* 2 x)))))
(check-expect (odds-first 5) (list 1 3 5 7 9))

;exercise 5

;powers-of-ten : Nat -> ListOfNumbers
;powers-of-ten constructs a list with the powers of ten in descending
;order by one
(define (powers-of-ten n)
  (build-list n (lambda (x) (cond [(= 0 x) 1]
                                [else (/ 1 (expt 10 x))]))))
(check-expect (powers-of-ten 4) (list 1 0.1 0.01 0.001))

;exercise 6

;diagonal : Nat -> ListOfListOfNumbers
;diagonal constructs a list of list of numbers with a given number
;of lists in it, where each list has 0's and 1's in a diagonal arrangement
(define (diagonal n)
  (build-list n (lambda (x)
                  (build-list n (lambda (y) (if (= x y) 1 0))))))
(check-expect (diagonal 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

;exercise 7

;random-between : Number Number Number -> ListOfNumber
;generates list of how-many numbers randomly chosen between low and high
(define (random-between low high how-many)
  (build-list how-many (lambda (x) (- (random (* 2 high)) low))))
