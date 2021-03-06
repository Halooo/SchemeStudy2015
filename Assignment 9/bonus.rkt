;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 09, Problem 4
;; ***************************************************
;;

(define (my-compose f g)
  (lambda (n) (f (g n))))

(define (curry f)
  (lambda (a b) (f a b)))
 
(define (uncurry f)
  (lambda (a b) ((f a) b)))


(define (eat-apples lst)
  (filter (lambda (x) (symbol=? x 'apple)) lst))

(define (my-map func lst)
  (foldr (uncurry (compose (curry cons) func)) empty lst))

