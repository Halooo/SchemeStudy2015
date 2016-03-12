;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wonderdiet) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 03, Problem 2
;; ***************************************************
;;


;; An Nutri-Info is a (make-nutri-info Num Num Num Num)
(define-struct nutri-info (serving-size fat carb prot))

;; (non-negative?) determine if a number (num-imput) is non-negative
;; non-negative: Num -> Bool
;; example:
(check-expect (non-negative? -10) false)
(check-expect (non-negative? 5) true)
(define (non-negative? num-input)
  (cond [(>= num-input 0) true]
        [else false]))

;; Nutri-Info Template 
;; some-nutri-info: Nutri-Info -> Any
(define (some-nutri-info ninfo)
  (... (nutri-info-serving-size ninfo) ...
     (nutri-info-fat ninfo) ...
     (nutri-info-carb ninfo) ...
     (nutri-info-prot ninfo) ... ))
;; (valid-nutri-info?) is a predicate function that take one parameter info
;;    and determine if info is a valid Nutri-info
;; valid-nutri-info?: Nutri-Info -> Bool
;; Example:
(check-expect (valid-nutri-info? (make-nutri-info 5 2 3 0)) true)
(check-expect (valid-nutri-info? (make-nutri-info 8 2 3 1)) true)

(define (valid-nutri-info? ninfo)
  (and (non-negative? (nutri-info-serving-size ninfo))
       (non-negative? (nutri-info-fat ninfo))
       (non-negative? (nutri-info-carb ninfo))
       (non-negative? (nutri-info-prot ninfo))
       (<= (+ (nutri-info-fat ninfo) (nutri-info-carb ninfo) (nutri-info-prot ninfo))
           (nutri-info-serving-size ninfo))))

;; Test:
(check-expect (valid-nutri-info? (make-nutri-info -1 2 3 0)) false)
(check-expect (valid-nutri-info? (make-nutri-info 5 8 1 0)) false)
(check-expect (valid-nutri-info? (make-nutri-info -27 -11 -5 -2)) false)
(check-expect (valid-nutri-info? (make-nutri-info 17 5 7 2)) true)

;; (higher-protein ninfo1 ninfo2) consumes two Nutri-infos (ninfo1 ninfo2)
;;    and produce the one with higher protein contentIf both have the same protein content, 
;;    it produces the Nutri-Info with the smaller serving size. If both the protein conten of
;;    the serving size are the same, it produces the first Nutri-Info
;; higher-protein: Nutri-Info Nutri-Info -> Nutri-Info
;; Example:
(check-expect (higher-protein (make-nutri-info 5 2 3 0) (make-nutri-info 5 1 2 3)) (make-nutri-info 5 1 2 3))
(check-expect (higher-protein (make-nutri-info 11 0 0 11) (make-nutri-info 5 1 2 3)) (make-nutri-info 11 0 0 11))
(define (higher-protein ninfo1 ninfo2)
  (cond [(< (nutri-info-prot ninfo2) (nutri-info-prot ninfo1)) ninfo1]
        [(= (nutri-info-prot ninfo2) (nutri-info-prot ninfo1))
         (cond [(< (nutri-info-serving-size ninfo1)
                (nutri-info-serving-size ninfo2)) ninfo1]
               [(= (nutri-info-serving-size ninfo1)
                (nutri-info-serving-size ninfo2)) ninfo1])]
        [else ninfo2]))
;; Test:
(check-expect (higher-protein (make-nutri-info 17 3 3 3) (make-nutri-info 19 5 5 3)) (make-nutri-info 17 3 3 3))
(check-expect (higher-protein (make-nutri-info 2 1 0 1) (make-nutri-info 0 0 0 0)) (make-nutri-info 2 1 0 1))
(check-expect (higher-protein (make-nutri-info 7 2 2 2) (make-nutri-info 5 1 2 3)) (make-nutri-info 5 1 2 3))
(check-expect (higher-protein (make-nutri-info 29 2 2 11) (make-nutri-info 29 3 3 11)) (make-nutri-info 29 2 2 11))

;; (combine-nutri-info ninfo1 ninfo2) takes 2 Nutri-Info (ninfo1 and ninfo2)
;;    and produce a new Nutri-Info that is the sum of both consumed values (info1 and info2 must be valid-nutri-info)
;; combine-nutri-info: Nutri-Info Nutri-Info -> Nutri-Info
;; Example:
(check-expect (combine-nutri-info (make-nutri-info 11 1 1 1) (make-nutri-info 17 3 2 5)) (make-nutri-info 28 4 3 6))
(define (combine-nutri-info ninfo1 ninfo2)
  (make-nutri-info (+ (nutri-info-serving-size ninfo1) (nutri-info-serving-size ninfo2))
                   (+ (nutri-info-fat ninfo1) (nutri-info-fat ninfo2))
                   (+ (nutri-info-carb ninfo1) (nutri-info-carb ninfo2))
                   (+ (nutri-info-prot ninfo1) (nutri-info-prot ninfo2))))
;; Test:
(check-expect (combine-nutri-info (make-nutri-info 0 0 0 0) (make-nutri-info 0 0 0 0)) (make-nutri-info 0 0 0 0))
(check-expect (combine-nutri-info (make-nutri-info 23 11 2 3) (make-nutri-info 17 11 2 3)) (make-nutri-info 40 22 4 6))

;; (good-combo?) consumes two Nutri-Infos and determines if the second Nutri-Info
;;    can be eaten (return true) in the hour following the firest Nutri-Info based on Charlatan's diet
;;    (info1 and info2 must be valid-nutri-info)
;; good-combo? : Nutri-Info Nutri-Info -> Bool
;; Example:
(check-expect (good-combo? (make-nutri-info 7 2 3 1) (make-nutri-info 5 2 1 1)) true)
(check-expect (good-combo? (make-nutri-info 19 1 1 1) (make-nutri-info 59 11 17 13)) false)
(define (good-combo? ninfo1 ninfo2)
  (and (<= (abs (- (nutri-info-fat ninfo1) (nutri-info-fat ninfo2))) 5)
       (<= (abs (- (nutri-info-carb ninfo1) (nutri-info-carb ninfo2))) 5)
       (<= (abs (- (nutri-info-prot ninfo1) (nutri-info-prot ninfo2))) 5)))

;; Test:
(check-expect (good-combo? (make-nutri-info 0 0 0 0) (make-nutri-info 5 5 5 5)) true)
(check-expect (good-combo? (make-nutri-info 0 0 0 0) (make-nutri-info 0 0 0 0)) true)
(check-expect (good-combo? (make-nutri-info 11 11 0 0) (make-nutri-info 17 2 7 7)) false)
(check-expect (good-combo? (make-nutri-info 19 2 3 11) (make-nutri-info 59 11 17 13)) false)
(check-expect (good-combo? (make-nutri-info 67 11 13 19) (make-nutri-info 23 5 7 11)) false)



  