;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname numbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 04, Problem 2
;; ***************************************************
;;

;; (sum-list lst) take a list of numbers (lst) and return the sum of all the elements in the list
;; sum-list: (listof Num) -> Num
;; Example:
(check-expect (sum-list (cons 2(cons 3(cons 4 empty)))) 9)
(check-expect (sum-list (cons 1(cons 0 empty))) 1)

(define (sum-list lst)
  (cond [(empty? lst) 0]
        [else (+ (first lst)
                 (sum-list (rest lst)))]))

;; Test
(check-expect (sum-list (cons 0(cons 0(cons 0 empty)))) 0)
(check-expect (sum-list empty) 0)
(check-expect (sum-list (cons 2 empty)) 2)
(check-expect (sum-list (cons 3(cons 1(cons 7 empty)))) 11)
(check-expect (sum-list (cons -13(cons 3(cons 1(cons 7 empty))))) -2)

;; (divide-list lst divisor) takes a list of number (lst) and a divisor, then return a list
;;    with all the elements divided by the divisor
;; divide-list: (listof Num) Num -> (listof Num)
;; Restriction: divisor must be non-zero
;; Example:
(check-expect (divide-list (cons 4 empty) 2)
              (cons 2 empty))
(check-expect (divide-list (cons 0(cons 0 empty)) 2)
              (cons 0(cons 0 empty)))

(define (divide-list lst divisor)
  (cond [(empty? lst) empty]
        [else (cons (/ (first lst) divisor)
                    (divide-list (rest lst) divisor))]))

;; Test:
(check-expect (divide-list empty 2) empty)
(check-expect (divide-list (cons 2(cons 3(cons 4 empty))) 2)
              (cons 1(cons 3/2(cons 2 empty))))
(check-expect (divide-list (cons 0(cons 0(cons 0 empty))) 2)
              (cons 0(cons 0(cons 0 empty))))
(check-expect (divide-list (cons -2(cons 3(cons 4 empty))) -2)
              (cons 1(cons -3/2(cons -2 empty))))

;; (normalize-list lst) takes a list of positive numbers and return the list obtained
;;    when dividing each element by the sum of all the elements of lst
;; normalize-list: (listof Num) -> (listof Num)
;; Reuqire: numbers in lst must be positive
;; Example:
(check-expect (normalize-list (cons 5 empty)) (cons 1 empty))
(check-expect (normalize-list (cons 5(cons 2 empty)))
              (cons 5/7(cons 2/7 empty)))
(define (normalize-list lst)
  (cond [(empty? lst) empty]
        [else (divide-list lst (sum-list lst))]))
(check-expect (normalize-list empty) empty)
(check-expect (normalize-list (cons 1(cons 2(cons 3 empty))))
              (cons 1/6(cons 2/6(cons 3/6 empty))))
(check-expect (normalize-list (cons 3(cons 5(cons 17 empty))))
              (cons 3/25(cons 1/5(cons 17/25 empty))))

;; (list-replace lst target-num replacement-num) takes a list of num (lst), a target-num,
;;     and a replacement-num then return a new list with any element in original list
;;     equal to the target-num being replaced with replacement-num
;; list-replace (listof Num) Num Num -> (listof Num)
;; Example:
(check-expect (list-replace (cons 3(cons 1(cons 3 empty))) 3 5)
              (cons 5(cons 1(cons 5 empty))))
(check-expect (list-replace (cons 0 empty) 1 5) (cons 0 empty))

(define (list-replace lst target-num replacement-num)
  (cond [(empty? lst) empty]
        [(= (first lst) target-num)
         (cons replacement-num
               (list-replace (rest lst) target-num replacement-num))]
        [else (cons (first lst)
                    (list-replace (rest lst) target-num replacement-num))]))
;; Test:
(check-expect (list-replace empty 0 0) empty)
(check-expect (list-replace (cons 0(cons 1(cons 0 empty))) 3 5)
              (cons 0(cons 1(cons 0 empty))))
(check-expect (list-replace (cons 0 empty) 0 1) (cons 1 empty))
(check-expect (list-replace (cons 7(cons 7(cons 5 empty))) 7 2)
              (cons 2(cons 2(cons 5 empty))))

;; (count-repeats lst) takes a list of int (lst), then return the number (int) of times that 
;;    the adjacent number of elements in the list that are equal
;; count-repeats: (listof Int) -> Int
;; Example:
(check-expect (count-repeats (cons 3(cons 3(cons 2 empty)))) 1)
(check-expect (count-repeats (cons 7(cons 3(cons 2 empty)))) 0)

(define (count-repeats lst)
  (cond [(empty? lst) 0]
        [(empty? (rest lst)) 0]
        [(= (first lst) (first (rest lst))) 
         (+ 1 (count-repeats (rest lst)))]
        [else (count-repeats (rest lst))]))
;; Test
(check-expect (count-repeats empty) 0)
(check-expect (count-repeats (cons 5 empty)) 0)
(check-expect (count-repeats (cons 3(cons 2 empty))) 0)
(check-expect (count-repeats (cons 3(cons 3 empty))) 1)
(check-expect (count-repeats (cons 3(cons 3(cons 3 empty)))) 2)
(check-expect (count-repeats (cons 3(cons 3(cons 3(cons 3 empty))))) 3)
(check-expect (count-repeats (cons 3(cons 1(cons 3(cons 3 empty))))) 1)
(check-expect (count-repeats (cons 3(cons 1(cons 3(cons 2 empty))))) 0)