;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

;A PrefixForest is a [NEListOf PrefixTree] 
;A PrefixTree is one of
; - (make-end)
; - (make-node 1String PrefixForest)
(define-struct end [])
(define-struct node [letter forest])

(define pt1
 (make-node "o"
            (list (make-node "n" (list
                                   (make-end)
                                   (make-node "e" (list (make-end)))))
                  (make-node "f" (list
                                   (make-end)
                                   (make-node "f" (list (make-end)))
                                   (make-node "t" (list (make-end)))))
                  (make-node "r" (list (make-end))))))
(define test_pt
  (make-node "w"
             (list
              (make-node "o"
                         (list
                          (make-node "n"
                                     (list
                                      (make-end))))))))
(define test_pt2
  (make-node "r"
             (list
              (make-node "a"
                         (list
                          (make-node "t"
                                     (list
                                      (make-end))))))))

;exercise 1

(define pt2
  (make-node "a"
             (list (make-node "t" (list
                                   (make-end)
                                   (make-node "e" (list (make-end)))))
                   (make-node "n" (list
                                   (make-end)
                                   (make-node "d" (list (make-end)))
                                   (make-end)))
                   (make-node "p" (list (make-end))))))
(define pt3
  (make-node "i"
             (list (make-node "c" (list
                                   (make-end)
                                   (make-node "e" (list (make-end)))))
                   (make-node "r" (list
                                   (make-end)
                                   (make-node "k" (list (make-end)))
                                   (make-end)))
                   (make-node "t" (list (make-end))))))
(define pf1 (list pt3))
(define pf2 (list pt2 pt3))
(define test_pf (list test_pt test_pt2))

;exercise 2

;alphabetize : PrefixForest -> PrefixForest
;alphabetize sorts the forest where each tree in it is also
;sorted alphabetically
(define (alphabetize forest)
  (cond [(empty? forest) empty]
        [else (cons (make-alpha-tree (sort (make-word (first forest)) string<?))
                    (alphabetize (rest forest)))]))
;(check-expect ...)

;make-word : PrefixTree -> Word
;make-word takes all the 1Strings and ends in the given tree and
;creates a word with them
(define (make-word tree)
  (cond [(end? tree) "/n"]
        [else (cons (node-letter tree) (make-word (node-forest tree)))]))

;make-alpha-tree : Word -> PrefixTree
;make-alpha-tree takes a sorted Word and creates a PrefixTree with it
(define (make-alpha-tree w)
  5)
;was completely unsure how to accomplish this

;exercise 3

;word-in-tree? : PrefixTree Word -> Boolean
;word-in-tree? takes a PrefixTree and determines if a given word
;is in the given tree
(define (word-in-tree? tr w)
  true)
(check-expect (word-in-tree? pt3 (list "i" "c" "e")) true)
;was completely unsure how to accomplish this

;word-in-forest? : PrefixForest Word -> Boolean
;word-in-forest? takes a PrefixForest and determines if a given word
;is in any of its trees
(define (word-in-forest? fr w)
  (cond [(empty? fr) false]
        [else (or (word-in-tree? (first fr) w) (word-in-forest? (rest fr) w))]))
(check-expect (word-in-forest? empty (list "i" "t")) false)
(check-expect (word-in-forest? pf1 (list "i" "c" "e")) true)
(check-expect (word-in-forest? pf2 (list "i" "r" "k")) true)

;exercise 4

;word->tree : Word -> PrefixTree
;word->tree takes a Word and returns a PrefixTree that stores exactly
;the given word
(define (word->tree w)
  (cond [(empty? w) (make-end)]
        [else (make-node (first w) (list (word->tree (rest w))))]))
(check-expect (word->tree (list "w" "o" "n")) test_pt)

;exercise 5

;add-to-tree : Word PrefixTree -> PrefixTree
;add-to-tree takes a word and a PrefixTree and adds the given word
;to the given PrefixTree
(define (add-to-tree w pt)
  5)
;was completely unsure how to accomplish this

;add-to-forest : Word PrefixForest -> PrefixForest
;add-to-forest takes a word and a PrefixForest and adds the given word
;to the given PrefixForest
(define (add-to-forest w pf)
  (cons (word->tree w) pf))
(check-expect (add-to-forest (list "w" "o" "n") pf1) (list test_pt pt3))

;exercise 6

;tree->list : PrefixTree -> [NEListOf String]
;tree->list takes a PrefixTree and returns a list of all strings
;stored in the tree
(define (tree->list pt)
  (cond [(end? pt) empty]
        [else (6helper (node-forest pt) (node-letter pt))]))
(check-expect (tree->list pt1) (list "one" "off" "oft" "or"))
(check-expect (tree->list (make-end)) empty)

(define (6helper pt str)
  (cond [(end? (first pt)) (list str empty)]
        [(not (empty? (rest pt))) (cons (6helper (node-forest (first pt))
                                                         (string-append str (node-letter (first pt))))
                                                (6helper (rest pt) str))]
        [else (6helper (node-forest (first pt))
                       (string-append str (node-letter (first pt))))]))

;forest->list : PrefixForest -> [NEListOf String]
;forest->list takes a PrefixForest and returns a list of all strings
;stored in all its trees
(define (forest->list pt)
  (cond [(empty? pt) empty]
        [else (cons (tree->list (first pt)) (forest->list (rest pt)))]))
(check-expect (forest->list pf1) (list "ice" "irk" "it"))

;exercise 7

;list->forest : [NEListOf String] -> PrefixForest
;list->forest takes a list of strings and returns a PrefixForest
;that contains trees which store all the words
(define (list->forest list)
  (cond [(empty? list) empty]
        [else (cons (word->tree (explode (first list))) (list->forest (rest list)))]))
(check-expect (list->forest (list "won" "rat")) test_pf)

;exercise 422

;list-chunks : List Nat -> List
;list-chunks takes a list of arbitrary data and a natural number and returns
;a list of list of chunks of a given size
;termination : each recursive call gives a list that is n smaller
(define (list-chunks l n)
  (cond [(empty? l) empty]
        [else (cons (sublist l n) (list-chunks (list-tail l n) n))]))
(check-expect (list-chunks (list 3 4 5 6 1 9 5 3 0 7 8 4) 3) (list (list 3 4 5)
                                                                   (list 6 1 9)
                                                                   (list 5 3 0)
                                                                   (list 7 8 4)))

;sublist : List Nat -> List
;sublist takes a list of arbitrary data and a natural number and returns
;a list of n elements from the given list
(define (sublist l n)
  (cond [(empty? l) empty]
        [(= n 0) empty]
        [else (cons (first l) (sublist (rest l) (- n 1)))]))

;list-tail : List Nat -> List
;list-tail takes a list of arbitrary data and a natural number and returns
;the list after the first n elements of the given list
(define (list-tail l n)
  (cond [(= n 0) l]
        [else (list-tail (rest l) (- n 1))]))

;exercise 433

;partition : String Nat -> ListOfString
;partition takes a string and a natural number and returns a list of substrings
;of the given string, each of a given length
;termination : each recursive call gives a string that is n smaller
(define (partition str n)
  (cond [(= 0 (string-length str)) empty]
        [else (cons (substring str 0 n) (partition (substring str n) n))]))
(check-expect (partition "This is a testing sentence." 3)
              (list "Thi" "s i" "s a" " te" "sti" "ng " "sen" "ten" "ce."))

;exercise 10

;tokenize : [ListOf 1String] -> [ListOf String]
;tokenize takes a ListOf1String and returns a list of strings where each string
;is made of lower-case letters that were next to each other in the input list, without
;any other characters or spaces
(define (tokenize l)
  (clean (tokenize1 l)))
(check-expect (tokenize (list "a" "b" " " "c" ";" "d" "e"))
              (list "ab" "c" "de"))
(check-expect (tokenize (list " " "-" " " ";" "d" "e"))
              (list "de"))
(check-expect (tokenize (list " " "-" " " ";" "d" "e" "(" ")"))
              (list "de"))
(check-expect (tokenize (list " " "-" " " ";" "#" "+"))
              (list))

(define (tokenize1 l)
  (cond [(empty? l) empty]
        [else (cons (tokenize2 l) (tokenize (tokenize3 l)))]))

(define (tokenize2 l)
  (cond [(empty? l) ""]
        [(or (and (string>=? "Z" (first l)) (string<=? "A" (first l)))
             (and (string>=? "z" (first l)) (string<=? "a" (first l))))
         (string-append (first l) (tokenize2 (rest l)))]
        [else ""]))

(define (tokenize3 l)
  (cond [(empty? l) empty]
        [(not (or (and (string>=? "Z" (first l))
                       (string<=? "A" (first l)))
                  (and (string>=? "z" (first l))
                       (string<=? "a" (first l))))) (rest l)]
        [else (tokenize3 (rest l))]))

(define (clean l)
  (cond [(empty? l) empty]
        [(string=? (first l) "") (clean (rest l))]
        [else (cons (first l) (clean (rest l)))]))
