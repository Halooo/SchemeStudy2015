;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstract) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 08, Problem 4
;; ***************************************************
;;

;; (avg-without-recursion lon) takes a lon (listof Nat) and produce a list with first
;;    element representing the average of all the numbers in lon, second element
;;    representing the average of the even numbers in lon, thrid element represents
;;    the average if the odd numbers in lon
;; avg-without-recursion: (listof Nat) -> (listof Nat)
;; require: lon must contain at least one odd and one even numbers
;; example:
(check-expect (avg-without-recursion (list 1 2)) (list 1.5 2 1))
(check-expect (avg-without-recursion (list 1 2 3 4 5 6 8)) (list 29/7 5 3))

(define (avg-without-recursion lon)
  (cons (/ (foldr + 0 lon) (length lon))
        (cons (/ (foldr + 0 (filter even? lon)) (length (filter even? lon)))
              (cons (/ (foldr + 0 (filter odd? lon)) (length (filter odd? lon))) empty))))

;; test:
(check-expect (avg-without-recursion (list 1 1 3 3 10 10)) (list 14/3 10 2))
(check-expect (avg-without-recursion (list 2 9 18 6 7 4 5)) (list 51/7 7.5 7))
(check-expect (avg-without-recursion (list 5 5 5 2 2 2)) (list 3.5 2 5))
(check-expect (avg-without-recursion (list 5 2 3 5 6 7 9 25)) (list 7.75 4 9))