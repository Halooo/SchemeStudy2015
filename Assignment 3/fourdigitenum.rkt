;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fourdigitenum) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 03, Problem 3
;; ***************************************************
;;

;; Constants:
(define e1-multiplier 1000)
(define e2-multiplier 100)
(define e3-multiplier 10)

;; (get-value-add1 pin)
;; get-value-add1: Four-Digit-Nat -> Num
;; Example:
(check-expect (get-value-add1 (make-four-digit-nat 1 2 3 4)) 1235)
(define (get-value-add1 pin)
  (+ (* e1-multiplier (four-digit-nat-e1 pin)) 
     (* e2-multiplier (four-digit-nat-e2 pin)) 
     (* e3-multiplier (four-digit-nat-e3 pin)) 
     (four-digit-nat-e4 pin) 1))

;; a Four-Digit-Nat is a (make-four-digit-nat Num Num Num Num)
(define-struct four-digit-nat (e1 e2 e3 e4))

;; (next-num pin) consumes a Four-Digit-Nat and produces the next
;;    larger Four-Digit-Nat
;; next-num: Four-Digit-Nat -> Four-Digit-Nat
;; Example:
(check-expect (next-num (make-four-digit-nat 0 0 0 0)) (make-four-digit-nat 0 0 0 1))
(check-expect (next-num (make-four-digit-nat 1 9 7 7)) (make-four-digit-nat 1 9 7 8))

(define (next-num pin)
  (cond [(= (four-digit-nat-e1 pin)
            (four-digit-nat-e2 pin)
            (four-digit-nat-e3 pin)
            (four-digit-nat-e4 pin) 9)
         (make-four-digit-nat 0 0 0 0)]
        [else
         (make-four-digit-nat
          (quotient (get-value-add1 pin) e1-multiplier)
          (quotient (modulo (get-value-add1 pin) e1-multiplier) e2-multiplier)
          (quotient (modulo (get-value-add1 pin) e2-multiplier) e3-multiplier)
          (modulo (get-value-add1 pin) e3-multiplier))]))

;; Test:
(check-expect (next-num (make-four-digit-nat 9 9 9 9)) (make-four-digit-nat 0 0 0 0))
(check-expect (next-num (make-four-digit-nat 1 0 0 0)) (make-four-digit-nat 1 0 0 1))
(check-expect (next-num (make-four-digit-nat 2 9 9 9)) (make-four-digit-nat 3 0 0 0))
(check-expect (next-num (make-four-digit-nat 2 5 8 7)) (make-four-digit-nat 2 5 8 8))

