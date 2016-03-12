;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 01, Problem 2
;; ***************************************************
;;

; Constants
(define oz-lb-converter 16)
(define lb-g-converter 453.59237)
(define ml-l-converter 1000)
(define floz-gallon-converter 128)
(define gallon-l-converter 3.78541)
;2 Unit Conversions
;2 (a) convert from oz to gram 
(define (oz->gram a)
  (* (/ a oz-lb-converter) lb-g-converter))
;2 (b) convert from ml to fluid oz
(define (ml->floz a)
  (* (/ (/ a ml-l-converter)
        gallon-l-converter)
     floz-gallon-converter))
;2 (c) convert from oz per fluid oz to grams per liter
(define (opo->gpl a)
  (/ (oz->gram a)
     (* gallon-l-converter
        (/ 1 floz-gallon-converter))))