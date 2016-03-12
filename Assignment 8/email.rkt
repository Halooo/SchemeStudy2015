;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname email) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 08, Problem 2
;; ***************************************************
;;

(define-struct email-record (day-id hours-worked emails-sent))
;; An Email-Record is a (make-email-record Nat Num Nat)
;; requires: hours-worked >= 0
(define-struct daily-stats (staff-id staff-name emails))
;; A Daily-Stats is a (make-daily-stats Nat Str (listof Email-Record))
;; requires: each day-id in emails is unique
(define stats1 (list (make-email-record 1 7 20) (make-email-record 2 8 50)
                     (make-email-record 3 6 30) (make-email-record 4 8 100)
                     (make-email-record 5 7 50)))
(define stats2 (list (make-email-record 1 10 80) (make-email-record 2 5 30)
                     (make-email-record 3 7 30) (make-email-record 4 7 80)
                     (make-email-record 5 12 80)))
(define stats3 (list (make-email-record 1 0 0)))
(define stats4 (list (make-email-record 1 2 1) (make-email-record 2 3 0)))
(define stats5 (list (make-email-record 1 12 10) (make-email-record 2 8 40)
                     (make-email-record 3 5 30) (make-email-record 4 5 40)
                     (make-email-record 5 10 80)))
(define stats6 empty)
(define em1 (make-daily-stats 1 "Justin" empty))
(define em2 (make-daily-stats 2 "Tolu" stats1))
(define em3 (make-daily-stats 10 "Alice" stats2))
(define em4 (make-daily-stats 4 "Bob" stats3))
(define em5 (make-daily-stats 5 "Cathy" stats4))
(define em6 (make-daily-stats 6 "Daniel" stats5))
(define em7 (make-daily-stats 7 "Emma" stats6))

;; (avg-emails ds) takes a ds (daily-stats) and return a number representing the
;;   average emails sent per hour for an employee
;; avg-emails: Daily-stats -> Num
;; example:
(check-expect (avg-emails em3) 300/41)
(check-expect (avg-emails em7) 0)
(define (avg-emails ds)
  (local [;; (ttl-email loes) takes a loes and return the number of total emails sent
          ;; ttl-email: (listof Email-Record) -> Nat
          (define (ttl-email loes)
            (cond [(empty? loes) 0]
                  [else (+ (email-record-emails-sent
                            (first loes))
                           (ttl-email (rest loes)))]))
          ;; (ttl-hr loes) takes a loes and return the number of total hours a staff worked
          ;; ttl-hr: (listof Email-Record) -> Nat
          (define (ttl-hr loes)
            (cond [(empty? loes) 0]
                  [else (+ (email-record-hours-worked
                            (first loes))
                           (ttl-hr (rest loes)))]))]
    (cond [(empty? (daily-stats-emails ds)) 0]
          [(zero? (ttl-hr (daily-stats-emails ds))) 0]
          [else (/ (ttl-email (daily-stats-emails ds))
                   (ttl-hr (daily-stats-emails ds)))])))
;; test:
(check-expect (avg-emails em1) 0)
(check-expect (avg-emails em2) 250/36)
(check-expect (avg-emails em4) 0)
(check-expect (avg-emails em5) 1/5)
(check-expect (avg-emails em6) 200/40)

;; (highest-email-record ds) takes a ds (daily-stats) and return a list of 
;;   email-records containing all the largest number of emails sent by a staff
;; highest-email-record: Daily-stats -> (listof Email-Record)
;; example:
(check-expect (highest-email-record em2)
              (list (make-email-record 4 8 100)
                    (make-email-record 5 7 50)))
(check-expect (highest-email-record em4) (list (make-email-record 1 0 0)))

(define (highest-email-record ds)
  (local [;; (email-lst loes) takes a loes and return a list of emails sent of all staff
          ;;   appeared in loes
          ;; email-lst: (listof Email-Record) -> (listof Nat)
          (define (email-lst loes)
            (cond [(empty? loes) empty]
                  [else (cons (email-record-emails-sent (first loes))
                              (email-lst (rest loes)))]))
          ;; (max-in-lst loes) gives the max in loes
          ;; max-in-lst: (listof Nat) -> Nat
          (define (max-in-lst loes)
            (foldr max 0 (email-lst loes)))
          ;; (search loes) takes a loes and return a list of Email-Records which has
          ;;   the same amount of email sent as the max in loes
          ;; search: (listof Email-Record) -> (listof Email-Record)
          (define (search loes)
            (cond [(empty? loes) empty]
                  [(= (email-record-emails-sent (first loes)) (max-in-lst loes))
                   (cons (first loes) (search (rest loes)))]
                  [else (search (rest loes))]))]
    (search (daily-stats-emails ds))))
;; test:
(check-expect (highest-email-record em1) empty)
(check-expect (highest-email-record em3)
              (list (make-email-record 1 10 80)
                    (make-email-record 4 7 80)
                    (make-email-record 5 12 80)))
(check-expect (highest-email-record em5)
              (list (make-email-record 1 2 1) (make-email-record 2 3 0)))
(check-expect (highest-email-record em6) (list (make-email-record 5 10 80)))
(check-expect (highest-email-record em7) empty)