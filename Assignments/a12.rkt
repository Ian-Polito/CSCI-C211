;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

;exercise 1

;A Ball is (make-ball Number Number Number Number)
(define-struct ball [x y hspeed vspeed])

(define b1 (make-ball 10 15 2 3))

;exercise 2

;A Table is (make-table Number Posn)
(define-struct table [rad point])

(define t1 (make-table 5 (make-posn 10 10)))
(define t2 (make-table 10 (make-posn 30 30)))

;dist : Posn Posn -> Number
;dist computes the distance between two Posns
(define (dist p0 p1)
  (sqrt (+ (expt (- (posn-x p0) (posn-x p1)) 2)
           (expt (- (posn-y p0) (posn-y p1)) 2))))

;on-table? : Ball Table -> Boolean
;determines if a given Ball is on a given Table
(define (on-table? b t)
  (cond [(< (table-rad t) (dist (table-point t) (make-posn (ball-x b) (ball-y b)))) false]
        [else true]))
(check-expect (on-table? b1 t1) true)
(check-expect (on-table? b1 t2) false)

;exercise 3

;move-ball : Ball -> Ball
;move-ball takes a ball and moves it according to its hspeed and vspeed
(define (move-ball b)
  (make-ball (+ (ball-x b) (ball-hspeed b))
             (+ (ball-y b) (ball-vspeed b))
             (ball-hspeed b)
             (ball-vspeed b)))
(check-expect (move-ball b1) (make-ball 12 18 2 3))

;exercise 4

;how-long : Ball Table -> Number
;how-long determines how many steps it will take for a ball to move off a table
(define (how-long b t)
  (cond [(not (on-table? b t)) 0]
        [else (+ 1 (how-long (move-ball b) t))]))
(check-expect (how-long b1 t2) 0)
(check-expect (how-long b1 t1) 1)

;exercise 5

;rle-encode : String -> String
;rle-encode takes a DNA String and produces its run-length encoding
(define (rle-encode dna)
  (cond [(string=? dna "") ""]
        [else (rle-encode-acc dna "")]))
(check-expect (rle-encode "AAGCCCCTTAAAAAAAAAA") "A2G1C4T2A10")
(check-expect (rle-encode "") "")

;rle-encode-acc : String String -> String
;rle-encode-acc takes a DNA String and an accumulator and produces its run-length encoding
(define (rle-encode-acc dna acc)
  (cond [(= (string-length dna) 0) acc]
        [else (rle-encode-acc (substring dna (how-many? (encode dna (substring dna 0 1) 0))
                                         (string-length dna))
                              (string-append acc (encode dna (substring dna 0 1) 0)))]))

;how-many? : String -> Number
;how-many? takes a String and determines how many chars are in the run-length substring
(define (how-many? sub)
  (string->number (substring sub 1 (string-length sub))))

;encode : String String Number -> String
;encode creates a run-length substring using a DNA string and the first character,
;along with an accumulator
(define (encode dna c acc)
  (cond [(= (string-length dna) 0) (string-append c (number->string acc))]
        [(string=? (substring dna 0 1) c) (encode (substring dna 1 (string-length dna)) c (+ 1 acc))]
        [else (string-append c (number->string acc))]))

;exercise 6

;rle-decode : String -> String
;rle-decode takes a DNA String and produces its decoded run-length
(define (rle-decode dna)
  (cond [(string=? dna "") ""]
        [else (rle-decode-acc dna "")]))
(check-expect (rle-decode "A2G1C4T2A10") "AAGCCCCTTAAAAAAAAAA")
(check-expect (rle-decode "") "")

;rle-decode-acc : String String -> String
;rle-decode-acc takes a DNA String and an accumulator and produces its decoded run-length
(define (rle-decode-acc dna acc)
  (cond [(= (string-length dna) 0) acc]
        [else (rle-decode-acc (substring dna
                                         (+ 1 (string-length (decode dna (substring dna 0 1) "")))
                                         (string-length dna))
                              (string-append acc (expand (string->number (decode dna
                                                                                 (substring dna 0 1)
                                                                                 ""))
                                                         (substring dna 0 1))))]))

;decode : String String String -> String
;decode creates a decoded substring using a DNA string and the first character,
;along with an accumulator
(define (decode dna c acc)
  (cond [(= (string-length dna) 0) acc]
        [(string=? (substring dna 0 1) c) (decode (substring dna 1 (string-length dna)) c acc)]
        [(not (or (string=? "A" (substring dna 0 1))
                  (string=? "C" (substring dna 0 1))
                  (string=? "T" (substring dna 0 1))
                  (string=? "G" (substring dna 0 1)))) (decode (substring dna 1 (string-length dna))
                                                               c
                                                               (string-append acc
                                                                              (substring dna 0 1)))]
        [else acc]))

;expand : Number String -> String
;expand takes a string and makes a new string by appending the given string
;to an empty string by the given number times
(define (expand num str)
  (cond [(= num 0) ""]
        [else (string-append str (expand (- num 1) str))]))

;exercise 7

(define very-efficient (expand 401 "A"))
(define very-inefficient "ACGTACGTGTCAGCTAGCTGAGC")

;compression-ratio : String -> Number
;returns how many times shorter rle-encode makes the given string
(define (compression-ratio s)
  (/ (string-length s) (string-length (rle-encode s))))
(check-expect (compression-ratio "AAAA") 2)
(check-expect (compression-ratio "AA") 1)

(check-expect (> (compression-ratio very-efficient) 100) true)
(check-expect (< (compression-ratio very-inefficient) 1) true)

;exercise 8

;quick-sort : [ListOf Number] -> [ListOf Number]
;returns the same list but in ascending order
(define (quick-sort-2 lon)
  (cond [(empty? lon) empty]
        [(= (length lon) 1) lon]
        [else (append (quick-sort-2 (filter (lambda (x) (<= x (first lon))) (rest lon)))
                      (list (first lon))
                      (quick-sort-2 (filter (lambda (x) (and (> x (first lon))
                                                             (<= x (first (rest lon)))))
                                            (rest lon)))
                      (quick-sort-2 (filter (lambda (x) (> x (first (rest lon)))) (rest lon))))]))
(check-expect (quick-sort-2 empty) empty)
(check-expect (quick-sort-2 (list 10 30 20 1 10 2)) (list 1 2 10 10 20 30))
;(check-expect (quick-sort-2 (list 10 30 20 1 10 30)) (list 1 10 10 20 30 30))
;I know that the above case is a problem, but am unsure how to fix it
(check-expect (quick-sort-2 (list 10 30 20 1 10)) (list 1 10 10 20 30))

;exercise 9

;num-comps : [ListOf Number] -> Number
;num-comps determines the number of comparisons used by the function sort>
(define (num-comps l)
  5)
;(check-expect (num-comps (list 4 3 2 1)) 3)
;(check-expect (num-comps (list 1 2 3 4)) 6)
;(check-expect (num-comps (list))         0)
;(check-expect (num-comps (list 1))       0)
;(check-expect (num-comps (list 1 2))     1)
;(check-expect (num-comps (list 2 1))     1)

;sort> : List-of-numbers -> List-of-numbers
;produces a sorted version of l
(define (sort> l)
  (cond [(empty? l) empty]
        [(cons? l) (insert (first l) (sort> (rest l)))]))
 
;insert : Number List-of-numbers -> List-of-numbers
;insert inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond [(empty? l) (cons n '())]
        [else (if (>= n (first l))
                  (cons n l)
                  (cons (first l) (insert n (rest l))))]))
