;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname complexmath) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 03, Problem 4
;; ***************************************************
;;

;; (posn-mult p1 p2) consumes two Posns (p1 and p2) and produces a
;;    Posn that is the result of the multiplication
;; posn-mult: Posn Posn -> Posn
;; Example:
(check-expect (posn-mult (make-posn 2 3) (make-posn 4 5)) (make-posn -7 22))
(check-expect (posn-mult (make-posn -1 -2) (make-posn -3 -4)) (make-posn -5 10))
(define (posn-mult p1 p2)
  (make-posn (- (* (posn-x p1) (posn-x p2)) (* (posn-y p1) (posn-y p2)))
             (+ (* (posn-x p1) (posn-y p2)) (* (posn-x p2) (posn-y p1)))))

;; Test:
(check-expect (posn-mult (make-posn 0 0) (make-posn 0 0)) (make-posn 0 0))
(check-expect (posn-mult (make-posn 0 0) (make-posn 5 7)) (make-posn 0 0))
(check-expect (posn-mult (make-posn 101 2) (make-posn 3 107)) (make-posn 89 10813))

;; (posn-div p1 p2) consumes two Posns (p1 and p2) and produces a
;;    Posn that is the result of dividing p1 by p2
;; posn-div: Posn Posn -> Posn
;; Restriction: p2 cannot be (make-posn 0 0)
;; Example:
(check-expect (posn-div (make-posn 1 2) (make-posn 1 1)) (make-posn 1.5 0.5))
(check-expect (posn-div (make-posn 10 10) (make-posn 10 10)) (make-posn 1 0))
(define (posn-div p1 p2)
  (make-posn (/ (+ (* (posn-x p1) (posn-x p2)) (* (posn-y p1) (posn-y p2)))
                (+ (sqr (posn-x p2)) (sqr (posn-y p2))))
             (/ (- (* (posn-y p1) (posn-x p2)) (* (posn-x p1) (posn-y p2)))
                (+ (sqr (posn-x p2)) (sqr (posn-y p2))))))
;; Test:
(check-expect (posn-div (make-posn 0 0) (make-posn 1 0)) (make-posn 0 0))
(check-expect (posn-div (make-posn 2 3) (make-posn 2 2)) (make-posn 1.25 0.25))

;; (rotate-along-circle angle psn) comsumes an angle and a posn (psn)
;;    to produce the posn after rotating conter-clockwise along the circle with radius
;;    sqrt(x^2+y^2) and center (0, 0) by the angle (angle)
;; rotate-along-circle: Num Posn -> Posn
;; Restriction: angle must between 0 and 2pi
;; Example:
(check-within (rotate-along-circle pi (make-posn 5 5)) (make-posn -5 -5) 0.1)

(define (rotate-along-circle angle psn)
  (posn-mult psn (make-posn (cos angle) (sin angle))))
;; Test:
(check-within (rotate-along-circle (/ pi 2) (make-posn 4 3)) (make-posn -3 4) 0.01)
(check-within (rotate-along-circle 1 (make-posn 0 0)) (make-posn 0 0) 0.1)