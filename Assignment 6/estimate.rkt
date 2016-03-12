;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname estimate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 06, Problem 2
;; ***************************************************
;;

(define random-number/big 4294967087)

;; (random-number min max) produces a random number between min and max
;; random-number: Num Num -> Num
;; Examples:
;; (random-number -1 1) -> 0.56
;; (random-number 0 0.5) -> 0.2

(define (random-number min max)
  (+ min (* (- max min) 
            (/ (random random-number/big) 
               random-number/big))))

;; (monte-carlo-helper n) takes n number of randomly generated points
;;    and put them into a square, return the number of points within
;;    the inscribed 1/4 circle
;; monte-carlo-helper: Nat -> Nat
;; example
;; (monte-carlo-helper 1000) -> 797
;; (monte-carlo-helper 91821) -> 72300

(define (monte-carlo-helper n)
  (cond [(zero? n) 0]
        [else
         (cond [(< (sqrt (+ (sqr (random-number 0 1))
                            (sqr (random-number 0 1)))) 1) 
                (+ 1 (monte-carlo-helper (- n 1)))]
               [else (monte-carlo-helper (- n 1))])]))
(monte-carlo-helper 1000)
;; (estimate-pi n) takes the number that monte-carlo estimated and compute pi with
;;    the number of points inside the 1/4 circle and the total number of points
;; estimate-pi: Nat -> Num
;; example
;; (estimate-pi 19230129174981269813081) -> 3.1415926
;; (estimate-pi 100000) -> 3.13864
;; (estimate-pi 987) -> 3.18541...
(define (estimate-pi n)
  (* 4 (/ (monte-carlo-helper n) n)))
(estimate-pi 1000)