;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname braverats) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 02, Problem 3
;; ***************************************************
;;

;; (braverats card-a card-b) takes two card numbers from player 'A and 'B in
;;    a braverat game and produce the winner of the game 'A or 'B or 'X if
;;    the game is on hold
;; braverats: Num Num -> Symbol
;; Example:
(check-expect (braverats 7 7) 'X)
(define (braverats card-a card-b)
  (cond [(or (= card-a card-b)
             (and (= card-a 0) (not (= card-b 5)))
             (and (= card-b 0) (not (= card-a 5)))) 'X]
        [(or (and (= card-a 7) (not (= card-b 1)))
             (and (= card-a 6) (not (or (= card-b 3) (= card-b 7))))
             (and (= card-a 5) (not (or (= card-b 6) (= card-b 7))))
             (and (= card-a 4) (not (or (= card-b 5) (= card-b 6) (= card-b 3) (= card-b 7))))
             (and (= card-a 3) (not (or (= card-b 5) (= card-b 1) (= card-b 2) (= card-b 7))))
             (and (= card-a 2) (not (or (= card-b 5) (= card-b 6) (= card-b 4) (= card-b 7))))
             (and (= card-a 1) (not (or (= card-b 5) (= card-b 6) (= card-b 2) (= card-b 4))))) 'A]
        [else 'B]))
;; Tests:
(check-expect (braverats 6 6) 'X)
(check-expect (braverats 5 0) 'A)
(check-expect (braverats 0 5) 'B)
(check-expect (braverats 7 0) 'X)
(check-expect (braverats 0 3) 'X)
(check-expect (braverats 3 6) 'A)
(check-expect (braverats 5 2) 'A)
(check-expect (braverats 2 1) 'A)
(check-expect (braverats 7 1) 'B)
(check-expect (braverats 6 7) 'B)
(check-expect (braverats 4 7) 'B)
(check-expect (braverats 1 4) 'B)