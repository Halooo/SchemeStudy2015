;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname offenders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 08, Problem 3
;; ***************************************************
;;

(define-struct email (sender-id recipient-ids word-count reply))
;; An Email is a (make-email Nat (listof Nat) Nat Reply)
;; a Reply is a (listof Email)

(define email7 (make-email 1 (list 2 3) 5 empty))
(define email6 (make-email 2 (list 1 4 5) 10 (list email7)))
(define email5 (make-email 3 (list 6 7) 15 (list email6)))
(define email4 (make-email 1 (list 2 3 4 5 6 7) 20 empty))
(define email3 (make-email 2 (list 3 5 6) 25 empty))
(define email2 (make-email 3 (list 4 7) 30 empty))
(define email1 (make-email 1 (list 2 3 4 5 6 7) 35 (list email2 email3 email4)))

;; (total-word-count e) takes an e (email) and reutrn the number of total words
;;   including the reply e-mails
;; total-word-count: Email -> Nat
;; example:
(check-expect (total-word-count email3) 25)
(check-expect (total-word-count email5) 25)
(check-expect (total-word-count email7) 5)
(define (total-word-count e)
  (local [(define (count-rep-word loe)
            (cond [(empty? loe) 0]
                  [else (+ (email-word-count (first loe))
                           (count-rep-word (rest loe)))]))]
    (cond [(empty? (email-reply e)) (email-word-count e)]
          [else (+ (email-word-count e) (count-rep-word (email-reply e)))])))
;; test:
(check-expect (total-word-count email1) 110)
(check-expect (total-word-count email6) 15)
(check-expect (total-word-count email4) 20)
(check-expect (total-word-count email2) 30)

;; (unique-email-senders loe) takes a loe and return a list of all non-repeating email
;;    senders in loe.
;; unique-email-senders: (listof Email) -> (listof Nat)
;; example:
(check-expect (unique-email-senders (list email2 email3)) (list 3 2))
(check-expect (unique-email-senders (list email2 email3 email5)) (list 3 2 1))

(define (unique-email-senders loe)
  (local [;; (find-sender email) takes an email and return a list of senders, including
          ;;   the senders in replied email
          ;; find-sender: Email -> (listof Nat)
          (define (find-sender email)
            (cond [(empty? (email-reply email)) (email-sender-id email)]
                  [else (cons (email-sender-id email)
                              (find-senders-lst (email-reply email)))]))
          ;; (find-senders-lst lst) is a helper to help find-sender to deal with a list
          ;;   of Email, return a list of senders
          ;; find-senders-lst: (listof Email) -> (listof Nat)
          (define (find-senders-lst lst)
            (cond [(empty? lst) empty]
                  [else (cons (find-sender (first lst))
                              (find-senders-lst (rest lst)))]))
          ;; (remove-repeat lst) takes a lst and remove repeating elements
          ;; remove-repeat: (listof Nat) -> (listof Nat)
          (define (remove-repeat lst)
            (cond [(empty? lst) empty]
                  [(member? (first lst) (rest lst)) (remove-repeat (rest lst))]
                  [else (cons (first lst) (remove-repeat (rest lst)))]))
          ;; (flatten lst) takes a nested list (lst) and return a flattened list
          ;; flatten: (listof Nat/(listof Nat)) -> (listof Nat)
          (define (flatten lst)
            (cond [(empty? lst) empty]
                  [(not (list? lst)) (list lst)]
                  [else (append (flatten (first lst)) (flatten (rest lst)))]))]
    (cond [(empty? loe) empty]
          [else 
           (remove-repeat
            (flatten (append (find-senders-lst loe)
                             (unique-email-senders (rest loe)))))])))
;; test:
(check-expect (unique-email-senders (list email6)) (list 2 1))
(check-expect (unique-email-senders (list email1 email2 email3 email4 email5 email6 email7)) (list 3 2 1))
(check-expect (unique-email-senders (list email7 email3)) (list 1 2))
(check-expect (unique-email-senders (list email1 email3)) (list 3 1 2))

;; (sent-email-summary loe) takes a loe and return a list of pairs whcih first element
;;   represents the id and second element of the pair represents the number it appears
;;   in the list of emails, including all the replied emails regardless of depth
;; sent-email-summary: (listof Email) -> (listof (listof Nat Nat))
;; example:
(check-expect (sent-email-summary (list email2))
              (list (list 3 1)))
(check-expect (sent-email-summary (list email3))
              (list (list 2 1)))

(define (sent-email-summary loe)
  (local [;; (find-sender email) takes an email and return a list of senders, including
          ;;   the senders in replied email
          ;; find-sender: Email -> (listof Nat)
          (define (find-sender email)
            (cond [(empty? (email-reply email)) (email-sender-id email)]
                  [else (cons (email-sender-id email)
                              (find-senders-lst (email-reply email)))]))
          ;; (find-senders-lst lst) is a helper to help find-sender to deal with a list
          ;;   of Email, return a list of senders
          ;; find-senders-lst: (listof Email) -> (listof Nat)
          (define (find-senders-lst lst)
            (cond [(empty? lst) empty]
                  [else (cons (find-sender (first lst))
                              (find-senders-lst (rest lst)))]))
          ;; (flatten lst) takes a nested list (lst) and return a flattened list
          ;; flatten: (listof Nat/(listof Nat)) -> (listof Nat)
          (define (flatten lst)
            (cond [(empty? lst) empty]
                  [(not (list? lst)) (list lst)]
                  [else (append (flatten (first lst)) (flatten (rest lst)))]))
          ;; (count-senders targ-id lst) takes a targ-id and a lst, return a number whcih
          ;;   records how many times targ-id appears in lst
          ;; count-senders: Nat (listof Nat) -> Nat
          (define (count-senders targ-id lst)
            (cond [(empty? lst) 0]
                  [(= targ-id (first lst)) (+ 1 (count-senders targ-id (rest lst)))]
                  [else (count-senders targ-id (rest lst))]))
          ;; (orig-lst loe) takes a loe and return the list of senders' id including repliers
          ;;   in loe, including repeated senders
          ;; orig-lst: (listof Email) -> (listof Nat)
          (define (orig-lst loe)
            (flatten (append (find-senders-lst loe)
                             (unique-email-senders (rest loe)))))
          ;; (sumry-lst unql origl) takes a unique sender list and the origl found in previous
          ;;   helper function and return a list containing the numbers of coresponding element
          ;;   in the unql at coresponding position
          ;; sumry-lst: (listof Nat) (listof Nat) -> (listof Nat)
          (define (sumry-lst unql origl)
            (cond [(empty? unql) empty]
                  [else (cons (count-senders (first unql) origl) (sumry-lst (rest unql) origl))]))
          ;; (combine-lsts l1 l2) takes 2 lists l1 and l2 and return the combined list as pairs
          ;;   which coresponding elements in l1 pairs with that in l2
          ;; combine-lsts: (listof Nat) (listof Nat) -> (listof (listof Nat))
          ;; require: l1 and l2 are the same length
          (define (combine-lsts l1 l2)
            (cond [(empty? l1) empty]
                  [else (cons (list (first l1) (first l2))
                              (combine-lsts (rest l1) (rest l2)))]))]
    (combine-lsts (unique-email-senders loe) (sumry-lst (unique-email-senders loe) (orig-lst loe)))
    ))

;; test:
(check-expect (sent-email-summary (list email1))
              (list (list 3 1) (list 2 1) (list 1 2)))
(check-expect (sent-email-summary (list email5))
              (list (list 3 1) (list 2 1) (list 1 1)))
(check-expect (sent-email-summary (list email6 email1))
              (list (list 3 2) (list 2 3) (list 1 4)))
(check-expect (sent-email-summary (list email1 email2 email3 email4 email5 email6 email7))
              (list (list 3 4) (list 2 5) (list 1 7)))

;; (email-offenders loe n) takes a loe and an n, find all the ones in loe who send more emails
;;   than the limit n and return a list of ids of them in non-decreasing order.
;; email-offenders: (listof Email) -> (listof Nat)
;; example:
(check-expect (email-offenders empty 1) '())
(check-expect (email-offenders (list email1) 1) (list 1))

(define (email-offenders loe n)
  (local [;; (search-offender lst) takes a list of pairs and find all thoes who send more 
          ;;   emails than limit n, returns their id in list without order
          ;; (search-offender: (listof (listof Nat)) -> (listof Nat)
          (define (search-offender lst)
            (cond [(empty? lst) empty]
                  [(> (second (first lst)) n)
                   (cons (first (first lst)) (search-offender (rest lst)))]
                  [else (search-offender (rest lst))]))]
    (cond [(empty? loe) empty]
          [else (sort (search-offender (sent-email-summary loe)) <=)])))
;; test:
(check-expect (email-offenders (list email1 email7 email6) 1) (list 1 2))
(check-expect (email-offenders (list email5) 0) (list 1 2 3))
(check-expect (email-offenders (list email1 email2 email3 email4 email5 email6 email7) 1) (list 1 2 3))
(check-expect (email-offenders (list email6 email1) 4) '())
(check-expect (email-offenders (list email1 email2 email3 email4 email5 email6 email7) 4) (list 1 2))