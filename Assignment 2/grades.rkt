;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 02, Problem 2
;; ***************************************************
;;

;; Variable explainations:
;; fstm is first midterm grade
;; scdm is second midterm grade
;; partic is participation grade
;; assig is assignment grade
;; finalex is final exam

;; Constant:
(define highest-grade-when-fail-assig-finalex 46)
(define grade-to-pass 50)

;; helper function
;; (final-grade fstm scdm practic assig finalex) calculates the final
;;    grade for a student through all components count for grades
;; final-grade: Num Num Num Num Num -> Num
;; Example:
(check-expect (final-grade 95 90 90 95 99) 95.55)
(define (final-grade fstm scdm practic assig finalex)
  (+ (* fstm 0.1)
     (* scdm 0.2)
     (* assig 0.2)
     (* practic 0.05)
     (* finalex 0.45)))
;; Tests:
(check-expect (final-grade 0 0 0 0 0) 0)
(check-expect (final-grade 81 80 19 11 60) 54.25)
(check-expect (final-grade 100 100 100 100 100) 100)

;; (cs135-final-grade fstm scdm practic assig finalex) produce the
;;    final grade in this course
;; cs135-final-grade Num Num Num Num Num -> Num
;; Example:
(check-expect (cs135-final-grade 45 55 50 30 20) 33)

(define (cs135-final-grade fstm scdm practic assig finalex)
  (cond [(or (< assig grade-to-pass)
             (< (/ (+ (* 0.45 finalex)
                      (* 0.1 fstm) (* 0.2 scdm)) 0.75) grade-to-pass))
         (min highest-grade-when-fail-assig-finalex (final-grade fstm scdm practic assig finalex))]
        [else (final-grade fstm scdm practic assig finalex)]))

;; TESTS:
(check-expect (cs135-final-grade 45 55 50 30 80) 46)
(check-expect (cs135-final-grade 47 53 29 67 91) 71.1)
(check-expect (cs135-final-grade 47 53 29 97 7) 39.3)