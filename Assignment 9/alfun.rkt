;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 09, Problem 4
;; ***************************************************
;;

;; (singletons lst) take a lst which is a list of numbers and return a list that
;;   contain all the elements only appeared once in lst
;; singletons: (listof Num) -> (listof Num)
;; example:
(check-expect (singletons '(1 1 1 1 2 2 3 4 5 5 6)) '(3 4 6))
(check-expect (singletons '(1 1 1 2 2 3 3 3 4 5 6 6)) '(4 5))

(define (singletons lst)
  (local [(define (appear-time lst n)
            (filter (lambda (x) (= x n)) lst))]
    (filter (lambda (x) (= 1 (length (appear-time lst x)))) lst)))
;; test:
(check-expect (singletons '(3 2 1)) '(3 2 1))
(check-expect (singletons '(33 22 11 3 2 3 33 11 2)) '(22))
(check-expect (singletons '()) '())
(check-expect (singletons '(1 1 1)) '())

;; (duplicates lst) take a lst which is a list of numbers and return a list that
;;   contain all the elements which has repeated in lst
;; duplicates: (listof Num) -> (listof Num)
;; example:
(check-expect (duplicates '(1 1 1 1 2 2 3 4 5 5 6)) '(1 2 5))
(check-expect (duplicates '(1 1 1 2 2 3 3 3 4 5 6 6)) '(1 2 3 6))

(define (duplicates lst)
  (local [(define (appear-time lst n)
            (filter (lambda (x) (= x n)) lst))]
    (foldr (lambda (x y)
             (cons x (filter
                      (lambda (z)
                        (not (= x z))) y)))
           empty
           (filter (lambda (x) (not (= 1 (length (appear-time lst x))))) lst))))

;; test
(check-expect (duplicates '(3 2 1)) '())
(check-expect (duplicates '(33 22 11 3 2 3 33 11 2)) '(33 11 3 2))
(check-expect (duplicates '()) '())
(check-expect (duplicates '(1 1 1)) '(1))

