;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 02, Problem 1
;; ***************************************************
;;

;; (a)
(define (q1a x)
  (cond
    [(and (p1? x) (p2? x)) 'up]
    [(and (p1? x) (not(p2? x))) 'down]
    [(p2? x) 'right]
    [else 'left]))

;; (b)
(define (q1b x)
  (cond
    [(p2? x) 'up]
    [else 'down]))

;; (c)
(define (q1c x)
  (cond
    [(and (p1? x) (p2? x) (p3? x)) 'up]
    [(and (p1? x) (p2? x) (not (p3? x))) 'down]
    [else 'right]))
