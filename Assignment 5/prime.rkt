;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname prime) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 05, Problem 1
;; ***************************************************
;;

;; (prime? p) is a predicater function that determine if a natrual number
;;    p is a prime number
;; prime?: Nat -> Bool
;; Require: p is Nat
;; Example:
(check-expect (prime? 3) true)
(check-expect (prime? 15) false)

(define (prime? p)
  (cond [(= p 1) false]
        [(= p 2) true]
        [else (prime-helper p 2)]))

; Test:
(check-expect (prime? 0) false)
(check-expect (prime? 1) false)
(check-expect (prime? 2) true)
(check-expect (prime? 27) false)
(check-expect (prime? 133) false)
(check-expect (prime? 9746347772161) false)

;; (prime-helper num1 i) determines wether a givin num1 divides all numbers
;;    smaller than num1; i is counter
;; prime-helper: Nat Nat -> Bool
;; require: num1 and i are Nat
;; example:
(check-expect (prime-helper 3 2) true)
(check-expect (prime-helper 27 2) false)
(check-expect (prime-helper 133 2) false)
(define (prime-helper num1 i)
  (cond [(= num1 i) true]
        [(not (= (modulo num1 i) 0))
         (prime-helper num1 (add1 i))]
        [else false]))

;; (next-prime n) takes a natural number n and return the next prime number
;;    greater than n
;; next-prime: Nat -> Nat
;; require: n is Nat
;; example:
(check-expect (next-prime 2) 3)
(check-expect (next-prime 577) 587)

(define (next-prime n)
  (cond [(prime? (add1 n)) (add1 n)]
        [else (next-prime (add1 n))]))
;; test:
(check-expect (next-prime 1) 2)
(check-expect (next-prime 0) 2)
(check-expect (next-prime 3) 5)
(check-expect (next-prime 311) 313)
(check-expect (next-prime 977) 983)

;; (prime-range n1 n2) takes n1 and n2 both natrual numbers and return a list
;;    of prime numbers in range of [n1, n2] the list contains primes greater than
;;    n1 and smaller than n2
;; prime-range: Nat Nat -> (listof Nat)
;; example:
(check-expect (prime-range 0 2) (list 2))
(check-expect (prime-range 13 2) empty)
(check-expect (prime-range 4 13) (list 5 7 11 13))

(define (prime-range n1 n2)
  (cond [(> n1 n2) empty]
        [(prime? n1)
         (cons n1 (prime-range (add1 n1) n2))]
        [else (prime-range (add1 n1) n2)]))
;; Test:
(check-expect (prime-range 1 2) (list 2))
(check-expect (prime-range 1 3) (list 2 3))
(check-expect (prime-range 65 99)
              (list 67 71 73 79 83 89 97))
(check-expect (prime-range 0 0) empty)
(check-expect (prime-range 1 1) empty)
(check-expect (prime-range 2 2) (list 2))
(check-expect (prime-range 2 1) empty)