;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/json)
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

;exercise 1

; read-json/web : String -> JSON
; Retrieves the remote file at the given URL and returns JSON data
 
; read-json/file : String -> JSON
; Retrieves the local file with the given name and returns JSON data

;exercise 2

; A JSON is one of:
; - (make-null)
; - Boolean
; - String
; - Number
; - [ListOf JSON]
; - (make-object [ListOf Member])
 
; A Member is [List String JSON]
;(define-struct null [])
;(define-struct object [members])

;parse : Anything -> JSON
;parse takes any input, ignores it, and returns the current
;data found in the doublemap bus map source
(define (parse input)
  (read-json/web "http://iub.doublemap.com/map/v2/buses"))

;exercise 3

#;
(define (process-JSON j)
  (cond [(null? j) ...]
        [(boolean? j) ...]
        [(string? j) ...]
        [(number? j) ...]
        [(object? j) (process-LoM (object-members j))]
        [else (process-LoJ j)]))

#;
(define (process-LoM j)
  (cond [(empty? j) ...]
        [else (process-mem (first j)) ... (process-LoM (rest j))]))

#;
(define (process-mem j)
  (cond [(empty? j) ...]
        [else (first j) ... (rest j)]))

#;
(define (process-LoJ j)
  (cond [(empty? j) ...]
        [else (process-JSON (first j)) ... (process-LoJ (rest j))]))

;exercise 4

;clean : JSON -> JSON
;clean takes a JSON and replaces every string with "censored"
(define (clean j)
  (cond [(list? j) (clean-list j)]
        [(string? j) "censored"]
        [(object? j) (make-object (clean-mem-list (object-members j)))]
        [else j]))
;(check-expect (clean "beep boop") "censored")
;(check-expect (clean (list "boop" #true 58 "yeller")) (list "censored" #true 58 "censored"))
;(check-expect (clean (list "boop" (list "jig" "saw") 56)) (list "censored" (list "censored" "censored") 56))
(check-expect (clean (make-object (list (list "same" "boop")))) (make-object (list (list "same" "censored"))))
;(check-expect (clean (list "tester" (make-object (list (list "same" "boop") (list "stay" (list "bip" "bop"))))))
              ;(list "censored" (make-object (list (list "same" "censored") (list "stay" (list "censored" "censored"))))))

;clean-list : [ListOf JSON] -> [ListOf JSON]
;clean-list takes a ListOfJSON and replaces every string with "censored"
(define (clean-list j)
  (cond [(empty? j) empty]
        [(string? (first j)) (cons "censored" (clean-list (rest j)))]
        [(object? (first j)) (cons (make-object (clean-mem-list (object-members (first j)))) (clean-list (rest j)))]
        [(list? (first j)) (cons (clean-list (first j)) (clean-list (rest j)))]
        [else (cons (first j) (clean-list (rest j)))]))

;clean-mem-list : [ListOf Member] -> [ListOf Member]
;clean-mem-list takes the members field of an object and censors all strings in each member's JSON
(define (clean-mem-list j)
  (cond [(empty? j) empty]
        [else (cons (clean-mem (first j)) (clean-mem-list (rest j)))]))

;clean-mem : Member -> Member
;clean-mem takes a member and censors all strings in its JSON
(define (clean-mem j)
  (list (first j) (clean (rest j))))

;exercise 5

