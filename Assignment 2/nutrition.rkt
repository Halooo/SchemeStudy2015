;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nutrition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 02, Problem 4
;; ***************************************************
;;

;; variable names explaination:
;; s-size is serving size
;; fat is fat content
;; carb is carbohydrate content
;; prot is protein content

;; Constants:
(define healthy-prot-gram 5)
(define fat-cal-cons 9)
(define prot-n-carb-cal-cons 4)
(define healthy-carb-gram 2)

;; (a)
;; (indigestible s-size fat carb prot) produces the number of
;;   grams in the serving that are not fat, carb or prot
;; indigestible Num Num Num -> Num
;; Examples:
(check-expect (indigestible  10 2 2 2) 4)
(check-expect (indigestible  5.5 1.1 2.1 0.3) 2)

(define (indigestible s-size fat carb prot)
  (- s-size (+ fat carb prot)))

;; Tests:
(check-expect (indigestible  17 7 3 11) -4)
(check-expect (indigestible  23 7 13 3) 0)

;; (b)
;; (high-protein? prot) determines if the number of grams of
;;   protein are at least five
;; high-protein? Num -> Bool
;; Examples:
(check-expect (high-protein? 0 0 0 1) false)
(check-expect (high-protein? 0 0 0 8) true)

(define (high-protein? s-size fat carb prot)
  (>= prot healthy-prot-gram))

;;tests
(check-expect (high-protein? 0 0 0 3) false)
(check-expect (high-protein? 0 0 0 5) true)

;; (c)
;; (calories s-size fat carb prot) produces the number of
;;   calories in the food
;; calories Num Num Num Num -> Num
;;Examples:
(check-expect (calories 0 2 5 11) 82)

(define (calories s-size fat carb prot)
  (+ (* fat fat-cal-cons)
     (* (+ carb prot) prot-n-carb-cal-cons)))

;;Tests:
(check-expect (calories 0 2 5 11) 82)
(check-expect (calories 0 2.2 5.1 11.9) 87.8)

;; (d)
;; (low-carb? fat carb prot) determines if the number of grams of carbohydrate
;;    are less than two
;; low-carb? Num Num Num -> Bool
;; Example:
(check-expect (low-carb? 0 0 1 1) true)
(define (low-carb? s-size fat carb prot)
  (cond [(and (>= (/ (* 4 carb) (calories 0 fat carb prot)) 0.05) (< carb 2)) true]
        [else false]))
;; tests:
(check-expect (low-carb? 0 1 1.9 1) true)
(check-expect (low-carb? 0 0 3 0) false)
(check-expect (low-carb? 0 100 3 10) false)

;; (e)
;; (zone? fat carb prot)  determines if a food is balanced for
;;   the Zone diet
;; zone? Num Num Num -> Bool
;; Example:
(check-expect (zone? 0 100 0 0) false)
(check-expect (zone? 0 10 30 22) true)
(define (zone? s-size fat carb prot)
  (cond [(= (calories 0 fat carb prot) 0) false]
        [(and (<= (abs (- (/ (* fat fat-cal-cons) (calories 0 fat carb prot)) 0.3)) 0.02)
              (<= (abs (- (/ (* carb prot-n-carb-cal-cons) (calories 0 fat carb prot)) 0.4)) 0.02)
              (<= (abs (- (/ (* prot prot-n-carb-cal-cons) (calories 0 fat carb prot)) 0.3)) 0.02)) true]
        [else false]))

;; Test:
(check-expect (zone? 0 7 21 17) true)
(check-expect (zone? 0 0 0 0) false)