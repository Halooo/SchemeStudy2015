;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname labinterface05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct game (winner loser high low))
;; A Game is a (make-game Str Str Nat Nat), where
;;      winner is the name of the winner of the game,
;;      loser is the name of the loser of the game,
;;      high is the winner's score, and
;;      low is the loser's score (high is greater than low).

(define-struct timer (hours mins secs))
;; A Timer is a (make-timer Nat Nat Nat), where
;;      hours is the number of hours,
;;      mins is the number of minutes in the range 0 - 59, and
;;      secs is the number of seconds in the range 0 - 59.

(define-struct card (value suit))
;; A Card is a (make-card Nat Sym), where
;;      value is the card value in the range 1 - 10 and
;;      suit is the card suit in the set 'hearts, 'diamonds, 'spades, and 'clubs.

(define-struct clock (hours mins))
;; A Clock is a (make-clock Nat Nat), where
;;      hours is the number of hours in the range 0 - 23 and
;;      mins is the number of minutes in the range 0 - 59.


;; Lab 05, Question 1

(define (fixed-game agame)
  ...)

;; Lab 05, Question 2

(define (convert-time t)
  ...)

;; Lab 05, Question 3

(define (bigger-card card1 card2)
  ...)

;; Lab 05, Question 4

(define (big-card-small-suit card1 card2)
  ...)

;; Lab 05, Question 5

(define (dur time1 time2)
  ...)

