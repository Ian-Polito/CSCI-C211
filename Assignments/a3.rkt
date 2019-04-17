;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define (format-month month size)
  (cond [(string=? "short" size)
         (cond [(string=? "January" month) "Jan"]
               [(string=? "February" month) "Feb"]
               [(string=? "March" month) "Mar"]
               [(string=? "April" month) "Apr"]
               [(string=? "May" month) "May"]
               [(string=? "June" month) "Jun"]
               [(string=? "July" month) "Jul"]
               [(string=? "August" month) "Aug"]
               [(string=? "September" month) "Sep"]
               [(string=? "October" month) "Oct"]
               [(string=? "November" month) "Nov"]
               [(string=? "December" month) "Dec"])]
        [(string=? "long" size) month]))

(check-expect (format-month "February" "short") "Feb")
(check-expect (format-month "March" "short") "Mar")
(check-expect (format-month "April" "short") "Apr")
(check-expect (format-month "April" "long") "April")
(check-expect (format-month "May" "short") "May")
(check-expect (format-month "June" "short") "Jun")
(check-expect (format-month "July" "short") "Jul")
(check-expect (format-month "August" "short") "Aug")
(check-expect (format-month "September" "short") "Sep")
(check-expect (format-month "October" "short") "Oct")
(check-expect (format-month "November" "short") "Nov")

(define (year-month-day->date year month day format length)
  (cond [(string=? format "MDY")
         (cond [(string=? length "short")
                (string-append (format-month month length) " "
                               (number->string day) ", " (number->string year))]
               [else (string-append month " " (number->string day) ", "
                                    (number->string year))])]
        [else (cond [(string=? length "short")
                (string-append (number->string day) " "
                               (format-month month length) " " (number->string year))]
               [else (string-append (number->string day) " "
                                    month " " (number->string year))])]))

(check-expect (year-month-day->date 1975 "November" 23 "MDY" "long") "November 23, 1975")
(check-expect (year-month-day->date 1984 "January" 1 "MDY" "short") "Jan 1, 1984")
(check-expect (year-month-day->date 2014 "October" 12 "DMY" "long") "12 October 2014")
(check-expect (year-month-day->date 67 "December" 29 "DMY" "short") "29 Dec 67")

(define (calendar year month day)
  (overlay (text (year-month-day->date year month day "MDY" "long") 32 "red") (empty-scene 500 100)))

(define (month->days-in-year month)
  (cond [(string=? month "January") 0]
        [(string=? month "February") 31]
        [(string=? month "March") 59]
        [(string=? month "April") 90]
        [(string=? month "May") 12]
        [(string=? month "June") 151]
        [(string=? month "July") 181]
        [(string=? month "August") 212]
        [(string=? month "September") 243]
        [(string=? month "October") 273]
        [(string=? month "November") 304]
        [(string=? month "December") 334]))

(define (year-month-day->days year month day)
  (cond [(string=? month "January") (+ (* year 365) (- day 1))]
        [(string=? month "February") (+ (* year 365) day 30)]
        [(string=? month "March") (+ (* year 365) day 58)]
        [(string=? month "April") (+ (* year 365) day 89)]
        [(string=? month "May") (+ (* year 365) day 119)]
        [(string=? month "June") (+ (* year 365) day 150)]
        [(string=? month "July") (+ (* year 365) day 180)]
        [(string=? month "August") (+ (* year 365) day 211)]
        [(string=? month "September") (+ (* year 365) day 242)]
        [(string=? month "October") (+ (* year 365) day 272)]
        [(string=? month "November") (+ (* year 365) day 303)]
        [(string=? month "December") (+ (* year 365) day 333)]))

(check-expect (year-month-day->days 0 "January" 1) 0)
(check-expect (year-month-day->days 2017 "August" 28) 736444)

(define (days-between year month day year2 month2 day2)
  (abs (- (year-month-day->days year month day) (year-month-day->days year2 month2 day2))))

(check-expect (days-between 2017 "June" 23 2017 "June" 25) 2)

(define (days->year days) (floor(/ days 365)))

(define (days-in-year->month days)
  (cond [(and (< -1 (remainder days 365)) (> 31 (remainder days 365))) "January"]
        [(and (< 30 (remainder days 365)) (> 59 (remainder days 365))) "February"]
        [(and (< 58 (remainder days 365)) (> 90 (remainder days 365))) "March"]
        [(and (< 89 (remainder days 365)) (> 120 (remainder days 365))) "April"]
        [(and (< 119 (remainder days 365)) (> 151 (remainder days 365))) "May"]
        [(and (< 150 (remainder days 365)) (> 181 (remainder days 365))) "June"]
        [(and (< 180 (remainder days 365)) (> 212 (remainder days 365))) "July"]
        [(and (< 211 (remainder days 365)) (> 243 (remainder days 365))) "August"]
        [(and (< 242 (remainder days 365)) (> 273 (remainder days 365))) "September"]
        [(and (< 272 (remainder days 365)) (> 304 (remainder days 365))) "October"]
        [(and (< 303 (remainder days 365)) (> 334 (remainder days 365))) "November"]
        [(and (< 333 (remainder days 365)) (> 365 (remainder days 365))) "December"]))

(check-expect (days-in-year->month 0) "January")
(check-expect (days-in-year->month 31) "February")
(check-expect (days-in-year->month 242) "August")

(define (days->month days)
  (cond [(and (< -1 (remainder days 365)) (> 31 (remainder days 365))) "January"]
        [(and (< 30 (remainder days 365)) (> 59 (remainder days 365))) "February"]
        [(and (< 58 (remainder days 365)) (> 90 (remainder days 365))) "March"]
        [(and (< 89 (remainder days 365)) (> 120 (remainder days 365))) "April"]
        [(and (< 119 (remainder days 365)) (> 151 (remainder days 365))) "May"]
        [(and (< 150 (remainder days 365)) (> 181 (remainder days 365))) "June"]
        [(and (< 180 (remainder days 365)) (> 212 (remainder days 365))) "July"]
        [(and (< 211 (remainder days 365)) (> 243 (remainder days 365))) "August"]
        [(and (< 242 (remainder days 365)) (> 273 (remainder days 365))) "September"]
        [(and (< 272 (remainder days 365)) (> 304 (remainder days 365))) "October"]
        [(and (< 303 (remainder days 365)) (> 334 (remainder days 365))) "November"]
        [(and (< 333 (remainder days 365)) (> 365 (remainder days 365))) "December"]))

(check-expect (days->month 59) "March")
(check-expect (days->month 364) "December")
(check-expect (days->month 736445) "August")

(define (days-in-year->days-in-month days)
  (cond [(string=? (days->month days) "January") (remainder days 365)]
        [(string=? (days->month days) "February") (- (remainder days 365) 31)]
        [(string=? (days->month days) "March") (- (remainder days 365) 59)]
        [(string=? (days->month days) "April") (- (remainder days 365) 90)]
        [(string=? (days->month days) "May") (- (remainder days 365) 120)]
        [(string=? (days->month days) "June") (- (remainder days 365) 151)]
        [(string=? (days->month days) "July") (- (remainder days 365) 181)]
        [(string=? (days->month days) "August") (- (remainder days 365) 212)]
        [(string=? (days->month days) "September") (- (remainder days 365) 243)]
        [(string=? (days->month days) "October") (- (remainder days 365) 273)]
        [(string=? (days->month days) "November") (- (remainder days 365) 304)]
        [(string=? (days->month days) "December") (- (remainder days 365) 334)]))

(check-expect (days-in-year->days-in-month 0) 0)
(check-expect (days-in-year->days-in-month 59) 0)
(check-expect (days-in-year->days-in-month 364) 30)

(define (days->day days)
  (cond [(string=? (days->month days) "January") (+ (remainder days 365) 1)]
        [(string=? (days->month days) "February") (- (remainder days 365) 30)]
        [(string=? (days->month days) "March") (- (remainder days 365) 58)]
        [(string=? (days->month days) "April") (- (remainder days 365) 89)]
        [(string=? (days->month days) "May") (- (remainder days 365) 119)]
        [(string=? (days->month days) "June") (- (remainder days 365) 150)]
        [(string=? (days->month days) "July") (- (remainder days 365) 180)]
        [(string=? (days->month days) "August") (- (remainder days 365) 211)]
        [(string=? (days->month days) "September") (- (remainder days 365) 242)]
        [(string=? (days->month days) "October") (- (remainder days 365) 272)]
        [(string=? (days->month days) "November") (- (remainder days 365) 303)]
        [(string=? (days->month days) "December") (- (remainder days 365) 333)]))

(check-expect (days->day 0) 1)
(check-expect (days->day 59) 1)
(check-expect (days->day 736324) 30)
(check-expect (days->day (year-month-day->days 1999 "December" 31)) 31)

(define init-year 0)
(define init-month "January")
(define init-day 1)
(define init-time 1)

(define (time-passing t)
  (calendar (days->year (* t init-time)) (days->month
                                               (* t init-time)) (days->day (* t init-time))))
(animate time-passing)