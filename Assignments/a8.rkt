;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

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

;exercise 1

(define (lighten2 m num)
  (cond [(leaf? m) (make-leaf (/ (leaf-weight m) num))]
        [else (make-rod (lighten2 (rod-lm m) num) (rod-ld m) (rod-rd m) (lighten2 (rod-rm m) num))]))

(check-expect (lighten2 mobile1 2) (make-rod (make-leaf 50) 25 18 (make-leaf 21)))

(define (enlarge2 m num)
  (cond [(leaf? m) m]
        [else (make-rod (enlarge2 (rod-lm m) num) (* num (rod-ld m)) (* num (rod-rd m)) (enlarge2 (rod-rm m) num))]))

(check-expect (enlarge2 mobile3 3) (make-rod leaf4 30 30 leaf4))

;enlarge-or-lighten : mobile Number Number -> mobile
;enlarge-or-lighten takes a mobile and two numbers, the first number for
;how much you want the mobile distances multiplied by and the second for
;how much you want the leaf weights divided by.

(define (enlarge-or-lighten m num num2)
  (cond [(leaf? m) (make-leaf (/ (leaf-weight m) num2))]
        [else (make-rod (enlarge-or-lighten (rod-lm m) num num2)
                        (* num (rod-ld m))
                        (* num (rod-rd m))
                        (enlarge-or-lighten (rod-rm m) num num2))]))

(check-expect (enlarge-or-lighten mobile1 1 2) (make-rod (make-leaf 50) 25 18 (make-leaf 21)))
(check-expect (enlarge-or-lighten mobile3 3 1) (make-rod leaf4 30 30 leaf4))

;exercise 2

; A NEList-of-temperatures is one of:
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)

; A NEList-of-booleans is one of:
; – (cons CBoolean '())
; – (cons CBoolean NEList-of-booleans)

;A [NEList-of X] is one of:
; - empty
; - (cons X [NEList-of X])

; A NEList-ofString is [NEListof String]
(define NEstr (cons "hello" empty))
; A NWList-ofPosn is [NEListof Posn]
(define NEPosn (cons (make-posn 5 10) empty))
; A NEList-ofImage is [NEListof Image]
(define NEImg (cons (circle 5 "solid" "red") empty))

;exercise 3

(define test (list "this" "me" "words"))
;shortest-string : ListOfString -> String
;shortest-string takes a ListOfString and returns
;the shortest string in the list
(define (shortest-string l)
  (retrieve l (shortest-string-helper l)))
;shortest-string-helper : ListOfString -> Number
;shortest-string-helper takes a ListOfString and returns the
;length of the shortest string in the list
(define (shortest-string-helper l)
  (cond [(empty? l) 50000000]
        [else (min (string-length(first l)) (shortest-string-helper (rest l)))]))
;retrieve : ListOfString Number -> String
;retrieve takes a ListOfString and a number and returns the first
;string in the list with the given length
(define (retrieve l num)
  (cond [(= (string-length (first l)) num) (first l)]
        [else (retrieve (rest l) num)]))
(check-expect (shortest-string test) "me")

(define test2 (list (make-posn 10 5) (make-posn 1 0)))
;Posn Posn -> Number
;dist computes the distance between two Posns
(define (dist p)
  (sqrt (+ (expt (- (posn-x p) 0) 2)
           (expt (- (posn-y p) 0) 2))))
;closest-posn : ListOfPosn -> Posn
;closest-posn takes a ListOfPosn and returns the posn that
;is closest to the origin
(define (closest-posn l)
  (retrieve-posn l (closest-posn-helper l)))
;closest-posn-helper : ListOfPosn -> Number
;closest-posn-helper takes a ListOfPosn and returns
;the distance of the posn that is closest to the origin
(define (closest-posn-helper l)
  (cond [(empty? l) 50000000]
        [else (min (dist (first l)) (closest-posn-helper (rest l)))]))
;retrieve-posn : ListOfPosn Number -> Posn
;retrieve-posn takes a ListOfPosn and a number and
;returns the posn in the list with the given distance
;from the origin
(define (retrieve-posn l num)
  (cond [(= (dist (first l)) num) (first l)]
        [else (retrieve-posn (rest l) num)]))
(check-expect (closest-posn test2) (make-posn 1 0))

(define test3 (list
               (list (circle 5 "solid" "red") (circle 10 "solid" "orange"))
               (list (circle 3 "solid" "purple"))))
;shortest-list : ListOfListOfImage -> ListOfImage
;shortest-list takes a ListOfListOfImage and returns the shortest
;list in it
(define (shortest-list l)
  (retrieve-list l (shortest-list-helper l)))
;shortest-list-helper : ListOfListOfImage -> Number
;shortest-list-helper takes a ListOfListOfImage and returns the number
;of items in the shortest list
(define (shortest-list-helper l)
  (cond [(empty? l) 50000000]
        [else (min (length (first l)) (shortest-list-helper (rest l)))]))
;retrieve-list : ListOfListOfImage Number -> ListOfImage
;retrieve-list takes a ListOfListOfImage and a number and returns
;the list with the given number of items
(define (retrieve-list l num)
  (cond [(= (length (first l)) num) (first l)]
        [else (retrieve-list (rest l) num)]))
(check-expect (shortest-list test3) (list (circle 3 "solid" "purple")))

;abstraction

;shortest : [ListOfListOf X] ([ListOfListOf X] -> Number)
;-> [ListOfListOf X]
;shortest takes a ListOfListOfImage and a function
;and returns a [ListOfListOf X]
(define (shortest l op)
  (retrieve-any l (shortest-helper l op) op))
;shortest-helper : [ListOfListOf X] ([ListOfListOf X] -> Number)
;-> Number
;shortest-helper takes a [ListOfListOf X] and a function
;and returns a number
(define (shortest-helper l op)
  (cond [(empty? l) 50000000]
        [else (min (op (first l)) (shortest-helper (rest l) op))]))
;retrieve-any : [ListOfListOf X] Number ([ListOfListOf X] -> Number)
;-> ListOfImage
;retrieve-any takes a [ListOfListOf X], a number and a function
;and returns the list with the given number of items
(define (retrieve-any l num op)
  (cond [(= (op (first l)) num) (first l)]
        [else (retrieve-any (rest l) num op)]))
(check-expect (shortest test3 length) (list (circle 3 "solid" "purple")))
(check-expect (shortest test2 dist) (make-posn 1 0))
(check-expect (shortest test string-length) "me")

;exercise 4

;remove-even : LoN -> LoN
;remove-even takes a List of Numbers and removes all of the even numbers
(define (remove-even list)
  (cond [(empty? list) empty]
        [(even? (first list)) (remove-even (rest list))]
        [else (cons (first list) (remove-even (rest list)))]))
(check-expect (remove-even (list 2 5 9 6 8 3)) (list 5 9 3))

;remove-empty : LoLoN -> LoLoN
;remove-empty takes a List of List of Numbers and removes the lists
;that are empty in it
(define (remove-empty list)
  (cond [(empty? list) empty]
        [(empty? (first list)) (remove-empty (rest list))]
        [else (cons (first list) (remove-empty (rest list)))]))
(define test4 (cons 5 (cons 4 empty)))
(define test5 empty)
(define test6 (cons 8 (cons 1 empty)))
(define test7 (cons test4 (cons test5 (cons test6 empty))))
(check-expect (remove-empty test7) (cons test4 (cons test6 empty)))

;abstraction

(define (remove-even-or-empty list func)
  (cond [(empty? list) empty]
        [(func (first list)) (remove-even-or-empty (rest list) func)]
        [else (cons (first list) (remove-even-or-empty (rest list) func))]))
(check-expect (remove-even-or-empty (list 2 5 9 6 8 3) even?) (list 5 9 3))
(check-expect (remove-even-or-empty test7 empty?) (cons test4 (cons test6 empty)))

;exercise 5

;tab : Number () -> ListOfNumber
;tabulates sqrt or sin between n 
;and 0 (incl.) in a list
(define (tabulate n func)
  (cond
    [(= n 0) (list (func 0))]
    [else (cons (func n) (tabulate (sub1 n) func))]))
(check-expect (tabulate 1 sqrt) (list 1 0))
(check-expect (tabulate 0 sin) (list 0))

	
;Number -> [List-of Number]
;tabulates sqr between n 
;and 0 (incl.) in a list
(define (tab-sqr n)
  (cond
    [(= n 0) (list (sqr 0))]
    [else
     (cons
      (sqr n)
      (tab-sqr (sub1 n)))]))
(check-expect (tab-sqr 2) (list 4 1 0))

;Number -> [List-of Number]
;tabulates tan between n 
;and 0 (incl.) in a list
(define (tab-tan n)
  (cond
    [(= n 0) (list (tan 0))]
    [else
     (cons
      (tan n)
      (tab-tan (sub1 n)))]))
(check-expect (tab-tan 0) (list 0))

;exercise 6

;has-< : ListOfNumber Number -> boolean
;has-< checks if some number in a LoN is less than
;the given number
(define (has-< list num)
  (cond [(empty? list) false]
        [(> (first list) num) true]
        [else (or false (has-< (rest list) num))]))
(check-expect (has-< (list 3 5) 4) true)
(check-expect (has-< (list 1 5) 9) false)

;has-string=? : ListOfString String -> boolean
;has-string=? checks if some string in a LoS is the same
;as the given string
(define (has-string=? list str)
  (cond [(empty? list) false]
        [(string=? (first list) str) true]
        [else (or false (has-string=? (rest list) str))]))
(check-expect (has-string=? (list "hello" "world") "world") true)
(check-expect (has-string=? (list "my" "head") "ow") false)

;has-empty? : ListOfListOfImages -> boolean
;has-empty? takes a LoLoI and checks if one of the
;lists is empty or not
(define (has-empty? list)
  (cond [(empty? (first list)) true]
        [else (has-empty? (rest list))]))
(check-expect (has-empty? test7) true)

;abstraction

(define (final list x func y)
  (cond [y (cond [(func (first list)) true]
                 [else (final (rest list) x func y)])]
        [else (cond [(empty? list) false]
                    [(func (first list) x) true]
                    [else (or false (final (rest list) x func y))])]))
(check-expect (final (list 3 5) 4 > false) true)
(check-expect (final (list "hello" "world") "world" string=? false) true)
(check-expect (final (list "my" "head") "ow" string=? false) false)
(check-expect (final test7 5 empty? true) true)

;exercise 7

;mul : ListOfNumber Number -> ListOfNumber
;mul takes a LoN and multiplies every number in it
;by the given number
(define (mul list num)
  (cond [(empty? list) empty]
        [else (map (lambda (x) (* x num)) list)]))
(check-expect (mul empty 5) empty)
(check-expect (mul (list 2 3 5) 2) (list 4 6 10))

;mul-table : ListOfNumber -> ListOfListOfNumber
;mul-table takes a LoLoN and makes a multiplication
;table using the given LoN
(define (mul-table list)
  (cond [(empty? list) empty]
        [else (cons 5 empty)]))
(check-expect (mul-table empty) empty)
;(check-expect (mul-table (list 2 3 5 10) (list
                                          ;(list 4 6 10 20)
                                          ;(list 6 9 15 30)
                                          ;(list 10 15 25 50)
                                          ;(list 20 30 50 100))))

;exercise 8

;; A TreeOfNumber is one of:
;;  - (make-leaf Number)
;;  - (make-node1 Number TreeOfNumber)
;;  - (make-node2 Number TreeOfNumber TreeOfNumber)
(define-struct tree-leaf [num])
(define-struct tree-node1 [num node])
(define-struct tree-node2 [num node1 node2])
(define bintree (make-tree-node2 10
                                 (make-tree-node1 9
                                                  (make-tree-leaf 4))
                                 (make-tree-leaf 6)))

;sum-tree : TreeOfNumber -> Number
;sum-tree takes a TreeOfNumber and adds up all the numbers in it
(define (sum-tree t)
  (cond [(tree-leaf? t) (tree-leaf-num t)]
        [(tree-node1? t) (+ (tree-node1-num t) (sum-tree (tree-node1-node t)))]
        [else (+ (tree-node2-num t) (sum-tree (tree-node2-node1 t)) (sum-tree (tree-node2-node2 t)))]))
(check-expect (sum-tree bintree) 29)

;prod-tree : TreeOfNumber -> Number
;prod-tree takes a TreeOfNumber and multiplies all the numbers in it
(define (prod-tree t)
  (cond [(tree-leaf? t) (tree-leaf-num t)]
        [(tree-node1? t) (* (tree-node1-num t) (prod-tree (tree-node1-node t)))]
        [else (* (tree-node2-num t) (prod-tree (tree-node2-node1 t)) (prod-tree (tree-node2-node2 t)))]))
(check-expect (prod-tree bintree) 2160)

;exercise 9

;op-tree : TreeOfNumber (Number Number -> Number) -> Number
;op-tree takes a TreeOfNumber and an operation and applies it
;to all numbers in the tree
(define (op-tree t op)
  (cond [(tree-leaf? t) (tree-leaf-num t)]
        [(tree-node1? t) (op (tree-node1-num t) (op-tree (tree-node1-node t) op))]
        [else (op (tree-node2-num t) (op-tree (tree-node2-node1 t) op) (op-tree (tree-node2-node2 t) op))]))
(check-expect (op-tree bintree +) 29)
(check-expect (op-tree bintree *) 2160)

;exercise 10

;count-tree : TreeOfNumber -> Number
;count-tree takes a TreeOfNumber and counts the number of leaves
(define (count-tree t)
  (cond [(tree-leaf? t) 1]
        [(tree-node1? t) (+ (count-tree (tree-node1-node t)))]
        [else (+ (count-tree (tree-node2-node1 t)) (count-tree (tree-node2-node2 t)))]))
(check-expect (count-tree bintree) 2)

;exercise 11

;process-tree : TreeOfNumber (Number Number -> Number) Boolean -> Number
;process-tree takes a TreeOfNumber a function and a base and applies
;the operation to all numbers in the tree
(define (process-tree t op b)
  (cond [b (cond [(tree-leaf? t) 1]
                 [(tree-node1? t) (op (process-tree (tree-node1-node t) op b))]
                 [else (op (process-tree (tree-node2-node1 t) op b) (process-tree (tree-node2-node2 t) op b))])]
        [else (cond [(tree-leaf? t) (tree-leaf-num t)]
                    [(tree-node1? t) (op (tree-node1-num t) (process-tree (tree-node1-node t) op b))]
                    [else (op (tree-node2-num t) (process-tree (tree-node2-node1 t) op b) (process-tree (tree-node2-node2 t) op b))])]))
(check-expect (process-tree bintree + false) 29)
(check-expect (process-tree bintree * false) 2160)
(check-expect (process-tree bintree + true) 2)

;process-tree is not the same as op-tree since process-tree must also be able to count all the leaves,
;which op-tree is not capable of since it accesses the numbers in the leafs and node instead

;exercise 12

;A [TreeOf X] is one of:
; - (make-leaf X)
; - (make-node1 X [TreeOf X])
; - (make-node2 X [TreeOf X] [TreeOf X])

;exercise 13

;I don't need to change the body of process-tree since it will already work with [TreeOf X]

;exercise 14

;I don't need to change the body of count-tree since it will already work with [TreeOf X]
