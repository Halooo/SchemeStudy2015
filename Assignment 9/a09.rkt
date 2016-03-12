;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 09, Problem 2 3
;; ***************************************************
;;

;; (intersection l1 l2) takes 2 lists, l1 and l2 and return a list with elements
;;   wich appeared in both l1 and l2, with no repeating element
;; intersection: (listof Any) (listof Any) -> (listof Any)
;; require: given l1 and l2 must be sets (with no repeating elemebts in it)
;; example:
(check-expect (intersection (list 'a 'b 'c 'd) (list 'c 'd)) '(c d))
(check-expect (intersection '(11 12 13) '(13 22 11)) '(11 13))

(define (intersection l1 l2)
  (filter (lambda (x) (member? x l2)) l1))
;; test: 
(check-expect (intersection '() '()) '())
(check-expect (intersection '(a) (list 'c 'd)) '())
(check-expect (intersection '(a 1 2 3) '(2 1 a)) '(a 1 2))
(check-expect (intersection '("hah" "banana" 2 3 4) '(7 8 9)) '())

;; (union l1 l2) takes 2 lists l1 and l2, return a list with all elements which
;;   appeared in l1 or l2. with no repeating elements
;; union: (listof Any) (listof Any) -> (listof Any)
;; example:
(check-expect (union (list 'a 'b 'c 'd) (list 'c 'd)) '(a b c d))
(check-expect (union '(11 12 13) '(13 22 11)) '(11 12 13 22))
(define (union l1 l2)
  (foldr (lambda (x y)
           (cons x (filter
                    (lambda (z)
                      (not (equal? x z))) y)))
         empty (foldr (lambda (x y) (foldr cons y x)) empty (list l1 l2))))
(check-expect (union '() '()) '())

;; (unique-fn lst pred) takes a list and a pred, returns a list with all repeat
;;   elements which satisfy the pred are removed
;; unique-fn: (listof Any) (Z -> Bool) -> (listof Any)
;; example:
(check-expect (unique-fn '(a a b b c d e) symbol=?) '(a b c d e))
(check-expect (unique-fn '(1 2 4 3 3) =) '(1 2 4 3))
(define (unique-fn lst pred)
  (foldr (lambda (x y)
           (cons x (filter
                    (lambda (z)
                      (not (pred x z))) y))) empty lst))
;; test:
(check-expect (unique-fn '(1 1.05 2 1.2)
                         (lambda (x y) (> 0.1 (abs (- x y))))) '(1 2 1.2))
(check-expect (unique-fn '() =) empty)
(check-expect (unique-fn '(0 1 2 3) (lambda (x y) (< x y))) '(0))
(check-expect (unique-fn '("hi" "i" "am" "Dave")
                         (lambda (x y) (string<? x y))) '("hi" "am" "Dave"))

;; (cross l1 l2) takes 2 lists l1 and l2 and return a list of list containing all
;;   posible combinations of elements in l1 and l2
;; cross: (listof Any) (listof Any) -> (listof (listof Any))
;; example:
(check-expect (cross '(1 2 3) '(4 5))
              (list '(1 4) '(1 5) '(2 4) '(2 5) '(3 4) '(3 5)))
(check-expect (cross '("hi" "hello") '("Tom" "Dave"))
              (list '("hi" "Tom") '("hi" "Dave") '("hello" "Tom") '("hello" "Dave")))

(define (cross l1 l2)
  (foldr (lambda (x y) (foldr cons y x)) empty
         ;(lambda (x y) (foldr cons y x)) is equivelent to append
         (map (lambda (x)
                (map (lambda (y)
                       (list x y)) l2)) l1)))
(check-expect
 (cross '(1 2 3 4) '(2 6))
 (list '(1 2) '(1 6) '(2 2) '(2 6) '(3 2) '(3 6) '(4 2) '(4 6)))
(check-expect (cross '() '()) '())
(check-expect (cross '(5 7 11) '(2)) (list '(5 2) '(7 2) '(11 2)))
(check-expect (cross '(2) '(a b c)) (list '(2 a) '(2 b) '(2 c)))

;; (jaccard l1 l2) takes l1 and l2 with same type and produces a number which
;;   indicates how similar l1 and l2 are.
;; jaccard: (listof X) (listof X) -> Num
;; require: l1 and l2 are same type
;; example:
(check-expect (jaccard '(1 2 3) '(1 2 3)) 1)
(check-expect (jaccard '(a b c d) '(z y c )) 1/6)
(define (jaccard l1 l2)
  (cond [(empty? (union l1 l2)) 1] ;(If A and B are
        ; both empty, we define J(A,B) = 1.) - wikiPedia/Jaccard_index
        [else (/ (length (intersection l1 l2))
                 (length (union l1 l2)))]))
;; test:
(check-expect (jaccard '() '()) 1)
(check-expect (jaccard '(1 2 3 4) '(2 5 1 6)) 1/3)
(check-expect (jaccard '(a b c) '(d e f)) 0)
(check-expect (jaccard '("hello") '()) 0)

;; (take lst n) takes a lst and a number n, return a list with the first n number
;;   of elements in the list
;; take: (listof Any) -> (listof Any)
;; example:
(check-expect (take '(1 2 3 4 5) 3) '(1 2 3))
(check-expect (take '(1 a b c d f) 5) '(1 a b c d))

(define (take lst n)
  (cond [(or (empty? lst) (zero? n)) empty]
        [else (cons (first lst)
                    (take (rest lst) (sub1 n)))]))

;; test
(check-expect (take '(1 2 3 4 5) 3) '(1 2 3))
(check-expect (take '() 3) '())
(check-expect (take '(a b 1 "c") 4) '(a b 1 "c"))
(check-expect (take '(5 6 a c) 0) '())


;; (cmp-with-sim dv fval simil) takes a DV, a FVAL and a predicator called simil, 
;;   return a list of DPTs for all the elements in FVAL.
;; cmp-with-sim: DV FV-AL ((listof X) (listof X) -> Num) -> (listof DPT)
;; require: dv must be non-empty
;; example:
(check-expect
 (cmp-with-sim (list "t1" '(a b c d))
               '(("t1" (a b c d)) ("t2" (e f g h))) jaccard)
 (list (list "t1" "t1" 1) (list "t1" "t2" 0)))
(check-expect
 (cmp-with-sim (list "t2" '(1 2 3 4))
               '(("t0" (a b c d)) ("t1" (1 2 3 4)) ("t2" (2 5 1 6))) jaccard)
 (list (list "t2" "t0" 0) (list "t2" "t1" 1) (list "t2" "t2" 1/3)))

(define (cmp-with-sim dv fval simil)
  (foldr (lambda (x y)
           (cons (list (first dv) (first x) (simil (second dv) (second x))) y))
         empty fval))
;; test:
(check-expect
 (cmp-with-sim (list "t1" '(1 2 3 4))
               '(("t1" (1 2 3 4)) ("t2" (2 5 1 6))) jaccard)
 (list (list "t1" "t1" 1) (list "t1" "t2" 1/3)))
(check-expect
 (cmp-with-sim (list "t1" '(1 2 3 4))
               '() jaccard) '())
(check-expect
 (cmp-with-sim (list "t1" '(1 2 3 4))
               '(("t1" (1 2 c c)) ("t2" (F i z 4))) jaccard)
 (list (list "t1" "t1" 2/5) (list "t1" "t2" 1/7)))
(check-expect
 (cmp-with-sim (list "t1" '(1 2 3 4))
               '(("t1" (1 2 3 4)) ("t2" (2 5 1 6))) jaccard)
 (list (list "t1" "t1" 1) (list "t1" "t2" 1/3)))

;; (find-all-exact fval simil) takes a FV-AL and a simil and return that according to
;;   the simil, a list containing the ones that are exactly the same ones repeating each other
;; find-all-exact: FV-AL ((listof X) (listof X) -> Num) -> (listof DPT)
;; example:
(check-expect
 (find-all-exact '(("a1" (41 1 2 3)) ("a2" (1 41 3 2)) ("a3" (2 3 41 1))) jaccard)
 (list (list "a1" "a2" 1) (list "a1" "a3" 1) (list "a2" "a1" 1) (list "a2" "a3" 1)
 (list "a3" "a1" 1) (list "a3" "a2" 1)))

(check-expect
 (find-all-exact '(("a1" (2 1 2 3)) ("a2" (1 41 3 2)) ("a3" (2 3 41 1))) jaccard)
 (list (list "a1" "a2" 1) (list "a1" "a3" 1) (list "a2" "a3" 1) (list "a3" "a2" 1)))

(define (find-all-exact fval simil)
  (filter (lambda (x)
            (and (not (string=? (first x) (second x))) (= (third x) 1)))
          (foldr (lambda (x y)
                   (foldr (lambda (x y) (foldr cons y x))
                          empty (list (cmp-with-sim x fval simil) y)))
                 empty fval)))
; test:
(check-expect
 (find-all-exact '(("t1" (1 2 3 4)) ("t2" (4 3 2 1)) ("t3" (5 1 2 4))) jaccard)
 (list (list "t1" "t2" 1) (list "t2" "t1" 1)))
(check-expect (find-all-exact '() jaccard) '())
(check-expect (find-all-exact '(("something" ("hi" "hello"))) jaccard) '())
(check-expect
 (find-all-exact '(("wut" (a b c d)) ("woot" (a c 1 4)) ("wat" (c a d b))) jaccard)
 (list (list "wut" "wat" 1) (list "wat" "wut" 1)))

;; (redundant? dpt1 dpt2) takes two DPTs dpt1 and dpt2, return true if the first two
;;   strings in dpt1 and dpt2 are redundant
;; redundant?: DPT DPT -> Bool
;; example:
(check-expect (redundant? '("t1" "t2" 1) '("t2" "t1" 1)) true)
(check-expect (redundant? '("haha" "yoyo" 1) '("haha" "yoyo" 1)) true)
(define (redundant? dpt1 dpt2)
  (or (and (string=? (first dpt1) (first dpt2))
           (string=? (second dpt1) (second dpt2)))
      (and (string=? (first dpt1) (second dpt2))
           (string=? (second dpt1) (first dpt2)))))
;; test
(check-expect (redundant? '("1" "t2" 10) '("t2" "1" 20)) true)
(check-expect (redundant? '("yes" "no" 1) '("yes" "yes" 1)) false)
(check-expect (redundant? '("yes" "no" 2) '("yes" "no" 3)) true)
(check-expect (redundant? '("100" "20" 11) '("100" "12" 37)) false)

;; (find-similar-within-d fval d simil) takes a FV-AL a d (threshold) and a simil,
;;   and return a list of DPTs that contain the third element greater than d
;; find-similar-within-d: FV-AL Num ((listof X) (listof X) -> Num) -> (listof DPT)
;; example:
(check-expect (find-similar-within-d
               '(("t1" (1 2 3 4))
                 ("t2" (5 3 2 1))
                 ("t3" (4 3 2 1))) .6 jaccard)
              (list (list "t1" "t3" 1)))
(check-expect (find-similar-within-d
               '(("t1" (1 2 3 4))
                 ("t2" (5 3 2 1))) 1 jaccard) '())

(define (find-similar-within-d fval d simil)
  (foldr (lambda (x y)
           (cons x (filter (lambda (z) (not (redundant? x z))) y))) empty
         (filter (lambda (x)
            (and (not (string=? (first x) (second x))) (> (third x) d)))
          (foldr (lambda (x y)
                   (foldr (lambda (x y) (foldr cons y x)) empty (list (cmp-with-sim x fval simil) y))) empty fval))))

(check-expect (find-similar-within-d '() .5 jaccard) '())
(check-expect (find-similar-within-d
               '(("t1" (1 2 3 4))
                 ("t2" (5 3 2 1))
                 ("t3" (4 3 2 1))
                 ("t4" (1 7 4 2))
                 ("t5" (11 13 17 37))) 0.2 jaccard)
              (list (list "t1" "t2" 6/10)
                    (list "t1" "t3" 1)
                    (list "t1" "t4" 6/10)
                    (list "t2" "t3" 6/10)
                    (list "t2" "t4" 1/3)
                    (list "t3" "t4" 6/10)))
(check-expect (find-similar-within-d
               '(("t1" (1 2 3 4))
                 ("t2" (5 3 2 1))).7 jaccard) '())
(check-expect (find-similar-within-d
               '(("a" (a c 1 4))
                 ("b" (c d 2 4))
                 ("c" (5 4 d a))) 0.3 jaccard)
              (list (list "a" "b" 1/3)
                    (list "a" "c" 1/3)
                    (list "b" "c" 1/3)))

;; (find-k-similar-within-d fval d k simil) takes a FV-AL a d (threshold) k and a simil,
;;   and return a list of first k DPTs that contain the third element greater than d
;; find-similar-within-d: FV-AL Num Nat ((listof X) (listof X) -> Num) -> (listof DPT)
;; example:
(check-expect (find-k-similar-within-d
               '(("t1" (1 2 3 4))
                 ("t2" (5 3 2 1))
                 ("t3" (4 3 2 1))).6 1 jaccard)
              (list (list "t1" "t3" 1)))
(check-expect (find-k-similar-within-d
               '(("t1" (1 2 3 4))
                 ("t2" (5 3 2 1))).5 0 jaccard) '())

(define (find-k-similar-within-d fval d k simil)
  (take (sort (foldr (lambda (x y)
           (cons x (filter (lambda (z) (not (redundant? x z))) y))) empty
         (filter (lambda (x)
            (and (not (string=? (first x) (second x))) (> (third x) d)))
          (foldr (lambda (x y)
                   (foldr (lambda (x y) (foldr cons y x)) empty (list (cmp-with-sim x fval simil) y))) empty fval)))
  (lambda (x y) (> (third x) (third y)))) k))
;; test
(check-expect (find-k-similar-within-d
               '(("t1" (1 2 3 4))
                 ("t2" (5 3 2 1))
                 ("t3" (4 3 2 1))).5 2 jaccard)
              (list (list "t1" "t3" 1) (list "t1" "t2" 6/10)))
(check-expect (find-k-similar-within-d '() .5 1 jaccard) '())
(check-expect (find-k-similar-within-d
               '(("t1" (1 2 3 4))
                 ("t2" (5 3 2 1))
                 ("t3" (4 3 2 1))
                 ("t4" (1 7 4 2))
                 ("t5" (11 13 17 37))) 0.2 5 jaccard)
              (list (list "t1" "t3" 1) 
                    (list "t1" "t2" 6/10)
                    (list "t1" "t4" 6/10)
                    (list "t2" "t3" 6/10)
                    (list "t3" "t4" 6/10)))
(check-expect (find-k-similar-within-d
               '(("t1" (1 2 3 4))
                 ("t2" (5 3 2 1))).7 3 jaccard) '())
(check-expect (find-k-similar-within-d
               '(("a" (a c 1 4))
                 ("b" (c d 2 4))
                 ("c" (5 4 d a))) 0.3 1 jaccard)
              (list (list "a" "b" 1/3)))