;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 01, Problem 1
;; ***************************************************
;;

; constant
(define g 9.8)
;1 (a) Surface area of torus
(define (doughnut-surface-area r z)
  (* (* (* 4 (sqr pi)) r) z))

;1 (b) Future value of investment
(define (future-value p r t)
  (* p (expt (+ 1 r) t)))

;1 (c) height of a thrown donut
(define (height v t)
  (- (* v t)
     (/ (* g(* t t)) 2)))
