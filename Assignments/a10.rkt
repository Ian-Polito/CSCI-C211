;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

;exercise 1

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

;heavy-leaves : [ListOf Mobile] Number -> Number
;heavy-leaves takes a ListOfMobile and a number and counts through the given
;list and counts how many leaves are heavier than the given weight
(define (heavy-leaves list num)
  (cond [(empty? list) 0]
        [else (+ (heavy-leaves-help (first list) num) (heavy-leaves (rest list) num))]))
(check-expect (heavy-leaves empty 30) 0)
(check-expect (heavy-leaves (list mobile1 mobile2) 0) 5)

;heavy-leaves-help : mobile Number -> Number
;hevay-leaves-help takes a mobile and a Number and returns the number of
;leaves in the mobile that are heavier than the given number
(define (heavy-leaves-help mobile num)
  (cond [(leaf? mobile) (cond [(> (leaf-weight mobile) num) 1]
                              [else 0])]
        [else (+ (heavy-leaves-help (rod-lm mobile) num) (heavy-leaves-help (rod-rm mobile) num))]))
(check-expect (heavy-leaves-help leaf1 10) 1)
(check-expect (heavy-leaves-help mobile1 50) 1)

;exercise 323

(define-struct no-info [])
;A no-info is (make-no-info)
(define-struct node [ssn name left right])
;A BinaryTree (short for BT) is one of:
; – no-info
; – (make-node Number Symbol BT BT)
(define NONE (make-no-info))
(define node1 (make-node 127 "Jake" NONE NONE))
(define node2 (make-node 33 "Emily" NONE NONE))
(define node3 (make-node 521 "Tobias" node1 node2))

;search-bt : Number BinaryTree -> String/Boolean
;search-bt searches through the given BT and if it contains a node where the ssn field
;is the same as the given Number, returns the value of the name field of that node,
;otherwise returns #false
(define (search-bt num bt)
  (cond [(contains-bt bt num) (search-bt-help num bt)]
        [else false]))
(check-expect (search-bt 33 node3) "Emily")
(check-expect (search-bt 500 node3) false)

;search-bt-help : Number BinaryTree -> String
;search-bt-help searches through the given BT and finds the node where its ssn field is the
;same as the given number and returns the name field of that node
(define (search-bt-help num bt)
  (cond [(no-info? bt) false]
        [(= num (node-ssn bt)) (node-name bt)]
        [(boolean? (search-bt-help num (node-left bt))) (search-bt-help num (node-right bt))]))

;contains-bt : BinaryTree Number -> Boolean
;searches through the given BT and returns true if a node's ssn field is the same
;as the given number
(define (contains-bt bt num)
  (cond [(no-info? bt) false]
        [(= num (node-ssn bt)) true]
        [else (or (contains-bt (node-left bt) num) (contains-bt (node-right bt) num))]))

;exercise 388

(define-struct employee [name ssn rate])
;An employee is (make-employee String Number Number)
(define-struct work-record [name hours])
;A work-record is (make-work-record String Number)
(define-struct wage-record [name wage])
;A wage-record is (make-wage-record String Number)

(define e1 (make-employee "Jake" 127 9))
(define e2 (make-employee "Duncan" 39 10))
(define e3 (make-employee "Pam" 54 8))
(define w1 (make-work-record "Jake" 40))
(define w2 (make-work-record "Duncan" 40))
(define w3 (make-work-record "Pam" 20))
(define emp-list (list e1 e2 e3))
(define rec-list (list w3 w1 w2))

;wages*.v2 : [ListOf employee] [ListOf work-record] -> [ListOf wage-record]
;wages*.v2 takes a ListOfemployee and a ListOfwork-record and creates a
;ListOfwage-record combining this information
(define (wages*.v2 emps recs)
  (cond [(empty? emps) empty]
        [else (cons (make-wage-record (employee-name (first emps))
                                      (* (employee-rate (first emps))
                                         (find-hours (employee-name (first emps)) recs)))
                    (wages*.v2 (rest emps) recs))]))
(check-expect (wages*.v2 emp-list rec-list) (list (make-wage-record "Jake" 360)
                                                  (make-wage-record "Duncan" 400)
                                                  (make-wage-record "Pam" 160)))

;find-hours : String [ListOf work-record] -> Number
;find-hours takes a name of an employee and searches through the given ListOfwork-record
;to find the work-record with the given name field and returns the number of hours
;worked of that work-record
(define (find-hours name rec)
  (cond [(empty? rec) 0]
        [(string=? name (work-record-name (first rec))) (work-record-hours (first rec))]
        [else (find-hours name (rest rec))]))
(check-expect (find-hours "Tammy" rec-list) 0)

;exercise 389

(define-struct phone-record [name number])
; A phone-record is (make-phone-record String String)

(define names1 (list "George" "Loid" "Manuel"))
(define phones1 (list "888-888-8888" "999-999-9999" "555-555-5555"))

;zip : ListOfString ListOfString -> [ListOf phone-record]
;zip takes a list of names and a list of phone numbers and combines this
;information to make a list of phone-records
(define (zip names phones)
  (cond [(empty? names) empty]
        [else (cons (make-phone-record (first names)
                                       (first phones))
                    (zip (rest names) (rest phones)))]))
(check-expect (zip names1 phones1) (list (make-phone-record "George" "888-888-8888")
                                         (make-phone-record "Loid" "999-999-9999")
                                         (make-phone-record "Manuel" "555-555-5555")))

;exercise 390

(define-struct branch [left right])
;A TreeOfSymbol is one of
; - Symbol
; - (make-branch TreeOfSymbol TreeOfSymbol)

;A Direction is one of
; - 'left
; - 'right

(define b1 (make-branch "@" (make-branch (make-branch "^" "%") "&")))
(define dirs (list 'right 'left 'right))

;tree-pick : TreeOfSymbol [ListOf Direction] -> TreeOfSymbol
;tree-pick takes a TreeOfSymbol and navigates through it according to the
;given ListOfDirection and returns the TreeOfSymbol when there are no
;more directions to take. If given a symbol and a non-empty ListOfDirection
;the function signals an error
(define (tree-pick tree dirs)
  (cond [(and (string? tree) (not (empty? dirs))) "Error: Given a symbol and a non-empty path."]
        [(empty? dirs) tree]
        [(equal? (first dirs) 'left) (tree-pick (branch-left tree) (rest dirs))]
        [else (tree-pick (branch-right tree) (rest dirs))]))
(check-expect (tree-pick "$" dirs) "Error: Given a symbol and a non-empty path.")
(check-expect (tree-pick b1 dirs) "%")

;exercise 2

(define-struct section (title text subsections))
;A Section is (make-section String [ListOf String] [ListOf Section])
;where the title field is the name of the section,
;the text field is the words in the section,
;and the subsections field is the contents of the section.

(define section1 (make-section "Contact Me"
                               (list "I" " can" " be" " reached" " at" " dummy@umail.iu.edu")
                               empty))
(define section2 (make-section "About"
                               (list "I" " am" " a" " student" " at" " Indiana" " University.")
                               section1))
(define section3 (make-section "My Name"
                               (list "Hello," " my" " name" " is" " Paul.")
                               section2))

;exercise 3

#;
(define (process-section s)
  (cond [(empty? s) ...]
        [else ... (section-title s) (section-text s) (process-section (section-subsections s))]))

;exercise 4

;search-section : section String -> ListOfString
;search-section takes a section and a word and returns a list of the names
;of each section which contains the given word in its text
(define (search-section s word)
  (cond [(empty? s) empty]
        [(search-text (section-text s) word) (cons (section-title s)
                                                   (search-section (section-subsections s) word))]
        [else (search-section (section-subsections s) word)]))
(check-expect (search-section empty "student") empty)
(check-expect (search-section section3 "wombat") empty)
(check-expect (search-section section3 "I") (list "About" "Contact Me"))

;search-text : ListOfString String -> Boolean
;search-text goes through the given list and returns true if
;any of them match the given string, otherwise returns false
(define (search-text text word)
  (cond [(empty? text) false]
        [(string=? word (first text)) true]
        [else (or false (search-text (rest text) word))]))
