;This was not written by me, this was written by my instructor during lecture.
;It is some sorting algorithm implementations I wanted to keep for reference

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 20171030process) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Old functions designed on November 30

; take : {X} [ListOf X] NaturalNumber -> [ListOf X]
; makes a list with the given number of elements
; using the beginning of the given list
; ** the given number must be at most the length of the given list **
(check-expect (take (list "hi" "bye" "211") 2)
              (list "hi" "bye"))
(check-expect (take (list "hi" "bye" "211") 0)
              empty)
(check-expect (take empty 0) empty)
(define (take l n)
  (cond [(positive? n) (cons (first l) (take (rest l) (sub1 n)))]
        [else empty]))

; merge : [ListOf Number] [ListOf Number] -> [ListOf Number]
; combines two sorted lists into a sorted list
(check-expect (merge (list 10 20 30) (list 1 2 10))
              (list 1 2 10 10 20 30))
(check-expect (merge (list 20 30) (list 1 2 10))
              (list 1 2 10 20 30))
(check-expect (merge (list 1 2 10) (list 10 20 30))
              (list 1 2 10 10 20 30))
(check-expect (merge empty (list 1 2 10))
              (list 1 2 10))
(check-expect (merge (list 1 10 20) empty)
              (list 1 10 20))
(define (merge l r)
  (cond [(empty? l) r]
        [(empty? r) l]
        [(< (first l) (first r))
         (cons (first l) (merge (rest l) r))]
        [else
         (cons (first r) (merge l (rest r)))]))


; drop : {X} [ListOf X] NaturalNumber -> [ListOf X]
; remove the first given-number elements of the given list
; ** the given number must be at most the length of the given list **
(check-expect (drop (list "hi" "bye" "211") 2) (list "211"))
(check-expect (drop (list "hi" "bye" "211") 0)
              (list "hi" "bye" "211"))
(define (drop l n)
  (cond [(positive? n) (drop (rest l) (sub1 n))]
        [else l]))

;;;;;;;;;;;;;;; November 1

; insert-sort : [ListOf Number] -> [ListOf Number]
; returns the same list but in ascending order
(check-expect (insert-sort empty) empty)
(check-expect (insert-sort (list 10 30 20 1 10 2)) (list 1 2 10 10 20 30))
(define (insert-sort lon)
  (cond [(empty? lon) empty]
        [(cons? lon)
         (insert (first lon) (insert-sort (rest lon)))]))

; insert : Number [ListOf Number] -> [ListOf Number]
; inserts the given number into the given sorted list where it belongs
; ** the given list must be sorted **
(check-expect (insert 10 empty) (list 10))
(check-expect (insert 10 (list 1 2 10 20 30)) (list 1 2 10 10 20 30))
(check-expect (insert 10 (list 20 30)) (list 10 20 30))
(define (insert n lon)
  (cond [(empty? lon) (list n)]
        [(cons? lon)
         (cond [(< (first lon) n) (cons (first lon) (insert n (rest lon)))]
               [else (cons n lon)])]))

;;;;;;;;;;;;;;; Generative recursion

; merge-sort : [ListOf Number] -> [ListOf Number]
; returns the same list but in ascending order
; termination: splitting a list of length >= 2 makes it shorter,
;              and eventually the length is <= 1
(check-expect (merge-sort empty) empty)
(check-expect (merge-sort (list 10 30 20 1 10 2)) (list 1 2 10 10 20 30))
(check-expect (merge-sort (list 10 30 20 1 10)) (list 1 10 10 20 30))
(define (merge-sort lon)
  (cond [(empty? lon) empty]
        [(empty? (rest lon)) lon]
        [else
         (merge (merge-sort (take lon (floor (/ (length lon) 2))))
                (merge-sort (drop lon (floor (/ (length lon) 2)))))]))

; The following is usually called "quick sort"
; append-sort : [ListOf Number] -> [ListOf Number]
; returns the same list but in ascending order
; termination: in a nonempty list, at least one element is equal to the first element,
;              so the lesser elements form a shorter list
;              and the greater elements form a shorter list;
;              in an empty list, we stop.
(check-expect (append-sort empty) empty)
(check-expect (append-sort (list 10 30 20 1 10 2)) (list 1 2 10 10 20 30))
(check-expect (append-sort (list 10 30 20 1 10)) (list 1 10 10 20 30))
(define (append-sort lon)
  (cond [(empty? lon) empty]
        [else
         (append (append-sort (filter (lambda (x) (< x (first lon))) lon))
                 (filter (lambda (x) (= x (first lon))) lon)
                 (append-sort (filter (lambda (x) (> x (first lon))) lon)))]))

; termination: (rest lon) is always shorter than lon,
;              so filtering it is yet shorter.
;              if lon is empty then we stop.
(check-expect (append-sort2 empty) empty)
(check-expect (append-sort2 (list 10 30 20 1 10 2)) (list 1 2 10 10 20 30))
(check-expect (append-sort2 (list 10 30 20 1 10)) (list 1 10 10 20 30))
(define (append-sort2 lon)
  (cond [(empty? lon) empty]
        [else
         (append (append-sort2 (filter (lambda (x) (<= x (first lon))) (rest lon)))
                 (list (first lon))
                 (append-sort2 (filter (lambda (x) (> x (first lon))) (rest lon))))]))




