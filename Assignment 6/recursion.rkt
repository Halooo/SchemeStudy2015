;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 06, Problem 1
;; ***************************************************
;;

;; (fibonacci-slow i) takes an index i and return the i-th fibonacci number
;; fibonacci-slow: Nat -> Nat
;; require: index cannot be too large
;; example
(check-expect (fibonacci-slow 1) 1)
(check-expect (fibonacci-slow 3) 2)

(define (fibonacci-slow i)
  (cond [(= i 0) 0]
        [(= i 1) 1]
        [else (+ (fibonacci-slow (- i 1)) (fibonacci-slow (- i 2)))]))
;; test
(check-expect (fibonacci-slow 2) 1)
(check-expect (fibonacci-slow 0) 0)
(check-expect (fibonacci-slow 7) 13)

;; (fibonacci-acc m n fm-2 fm-1) takes a starting value m (usually be 1)
;;    m is the current number being calculated, n is the final number as result
;;    and 2 base numbers fm-1 (previous fib number) and fm-2 (fib # after fm-1)
;;    and return the fibonacci number have been calculated
;; fibinacci-acc: Nat Nat Nat Nat -> Nat
;; example:
(check-expect (fibonacci-acc 1 10 1 0) 55)
(check-expect (fibonacci-acc 1 23 1 0) 28657)
(define (fibonacci-acc m n fm-2 fm-1)
  (cond [(= m n) (+ fm-2 fm-1)]
        [else (fibonacci-acc (add1 m) n fm-1 (+ fm-1 fm-2))]))

;; (fibonacci-fast i) takes an index i and return the i-th fibonacci number,
;;    index number can be large
;; fibonacci-fast: Nat -> Nat
;; example
(check-expect (fibonacci-fast 10) 55)
(check-expect (fibonacci-fast 23) 28657)

(define (fibonacci-fast i)
  (cond [(= i 0) 0]
        [(= i 1) 1]
        [else (fibonacci-acc 1 i 1 0)]))

(check-expect (fibonacci-fast 0) 0)
(check-expect (fibonacci-fast 1) 1)
(check-expect (fibonacci-fast 2) 1)
(check-expect (fibonacci-fast 3) 2)
(check-expect (fibonacci-fast 7) 13)
(check-expect (fibonacci-fast 131)
              1066340417491710595814572169)

;; (my-list-ref lst n) takes a list and a nature number n, returns the n-th
;;    element in the list
;; my-list-ref (listof Any) Nat -> Any
;; require: n is less than length of lst
;; example
(check-expect (my-list-ref (list 1 2 3 4 5) 1) 2)
(check-expect (my-list-ref (list 'a 'b 'c) 0) 'a)
(define (my-list-ref lst n)
  (cond [(empty? lst) empty]
        [(zero? n) (first lst)]
        [else (my-list-ref (rest lst) (sub1 n))]))
;; test
(check-expect (my-list-ref '() -1) '())
(check-expect (my-list-ref '() 0) '())
(check-expect (my-list-ref (list 'a 'b 'c) 0) 'a)
(check-expect (my-list-ref (list 'a 'b 'c) 2) 'c)

;; (str-of-char-helper n c) takes a number n and a character
;;    returns a list of character with n cs in it
;; str-of-char-helper: Nat Char -> (listof Char)
;; example:
(check-expect (str-of-char-helper 5 #\e)
              (cons #\e (cons #\e (cons #\e (cons #\e (cons #\e empty))))))
(check-expect (str-of-char-helper 1 #\space) (cons #\space empty))
(check-expect (str-of-char-helper 6 #\L)
              (cons #\L (cons #\L (cons #\L (cons #\L (cons #\L (cons #\L empty)))))))
(define (str-of-char-helper n c)
  (cond [(zero? n) empty]
        [else (cons c (str-of-char-helper (sub1 n) c))]))

;; (str-of-char n c) takes a number n and a character
;;    returns a string with n cs in it
;; str-of-char: Nat Char -> Str3
;; example
(check-expect (string-of-char 1 #\2) "2")
(check-expect (string-of-char 5 #\M) "MMMMM")
(define (string-of-char n c)
  (list->string (str-of-char-helper n c)))
;; test
(check-expect (string-of-char 2 #\space) "  ")
(check-expect (string-of-char 5 #\e) "eeeee")
(check-expect (string-of-char 3 #\K) "KKK")
(check-expect (string-of-char 1 #\,) ",")


(define (n-xs n)
  (cond [(zero? n) empty]
        [else (cons #\x (n-xs (sub1 n)))]))

;; (replace-vowels-helper lst) takes a lst of char and return a list
;;    character with all vowels being replaced by x
;; replace-vowels-helper: (listof Char) -> (listof Char)
;; example
(check-expect (replace-vowels-helper (list #\a) 1) (cons #\x empty))
(check-expect (replace-vowels-helper (list #\z) 1) (cons #\z empty))

(define (replace-vowels-helper lst n)
  (cond [(empty? lst) empty]
        [(or
          (char=? (first lst) #\a)
          (char=? (first lst) #\e)
          (char=? (first lst) #\i)
          (char=? (first lst) #\o)
          (char=? (first lst) #\u)
          (char=? (first lst) #\A)
          (char=? (first lst) #\E)
          (char=? (first lst) #\I)
          (char=? (first lst) #\O)
          (char=? (first lst) #\U))
         (append (n-xs n) (replace-vowels-helper (rest lst) (add1 n)))]
        [else (cons (first lst) (replace-vowels-helper (rest lst) n))]))

;; (replace-vowels str) takes a str and return a
;;    string with all vowels being replaced by x
;; replace-vowels: Str -> Str
;; require: input must be a string and not null
;; example:
(check-expect (replace-vowels "LION KING") "LxxxN KxxxNG")
(check-expect (replace-vowels "oh my world") "xh my wxxrld")
(define (replace-vowels str)
  (list->string (replace-vowels-helper (string->list str) 1)))

;; Test
(check-expect (replace-vowels "hellow world") "hxllxxw wxxxrld")
(check-expect (replace-vowels "yEEhA") "yxxxhxxx")
(check-expect (replace-vowels "hi wood") "hx wxxxxxd")


