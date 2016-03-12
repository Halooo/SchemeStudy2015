;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 01, Problem 3
;; ***************************************************
;;

; fstm is first midterm grade
; scdm is second midterm grade
; partic is participation grade
; assig is assignment grade
; 3(a)
(define (cs135-grade-sofar fstm scdm partic assig)
  (/ (+ (* fstm 0.1)
        (* scdm 0.2)
        (* assig 0.2)
        (* partic 0.05)) 0.55))

; grd1 is the grade before final exam
; grdfinal is the grade after final exam
; 3(b)
(define (cs135-final-exam grd1 grdfinal)
  (/ (- grdfinal
        (* grd1 0.55)) 0.45))