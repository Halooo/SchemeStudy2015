;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 04, Problem 4
;; ***************************************************
;;

(define-struct card(strength speed intelligence charm))
;; a Card is a (make-card Int Int Int Int) and all 4 Int>0

;; (card-to-list some-card) takes a Card (some-card) and return a list with its
;;    four elements. The order is: strength, speed, intelligence and charm
;; card-to-list: Card -> (listof Int)
;; Example
(check-expect (card-to-list (make-card 2 3 4 5)) (cons 2(cons 3(cons 4(cons 5 empty)))))
(check-expect (card-to-list (make-card 0 0 0 0)) (cons 0(cons 0(cons 0(cons 0 empty)))))
(define (card-to-list some-card)
  (cons (card-strength some-card)
        (cons (card-speed some-card)
              (cons (card-intelligence some-card)
                    (cons (card-charm some-card) empty)))))
;; Test:
(check-expect (card-to-list (make-card 17 53 91 19)) (cons 17(cons 53(cons 91(cons 19 empty)))))
(check-expect (card-to-list (make-card 1 1 1 1)) (cons 1(cons 1(cons 1(cons 1 empty)))))
(check-expect (card-to-list (make-card 9 9 9 9)) (cons 9(cons 9(cons 9(cons 9 empty)))))

;; (list-to-card lst) takes a list of 4 positive integers (lst) and return a Card with
;;    the four integers representing strength, speed, intelligence and charm in order
;; list-to-card: (listof Int) -> Card
;; Require: lst must contain exact 4 positive integers
;; Example:
(check-expect (list-to-card (cons 1(cons 1(cons 1(cons 1 empty))))) (make-card 1 1 1 1))
(check-expect (list-to-card (cons 2(cons 3(cons 4(cons 5 empty))))) (make-card 2 3 4 5))
(define (list-to-card lst)
  (make-card (first lst) (first (rest lst))
             (first (rest (rest lst)))
             (first (rest (rest (rest lst))))))
; Test
(check-expect (list-to-card (cons 0(cons 0(cons 0(cons 0 empty)))))
              (make-card 0 0 0 0))
(check-expect (list-to-card (cons 100(cons 20(cons 50(cons 210 empty)))))
              (make-card 100 20 50 210))

;; (sum-list lst) is a helper function
;;    it takes a list of numbers (lst) and return the sum of all the elements in the list
;; sum-list: (listof Num) -> Num
;; Example:
(check-expect (sum-list (cons 2(cons 3(cons 4 empty)))) 9)
(check-expect (sum-list (cons 1(cons 0 empty))) 1)
(define (sum-list lst)
  (cond [(empty? lst) 0]
        [else (+ (first lst)
                 (sum-list (rest lst)))]))

;; (card-regular? some-card) is a predicator function, it takes a Card (some-card) and
;;    return true if the sum of its strength, speed, intelligence and charm is equal to 10
;;    return false otherwise
;; card-regular?: Card -> Bool
;; Example:
(check-expect (card-regular? (make-card 9 9 9 9)) false)
(check-expect (card-regular? (make-card 1 2 3 4)) true)
(define (card-regular? some-card)
  (cond [(= (sum-list (card-to-list some-card)) 10) true]
        [else false]))
;; test:
(check-expect (card-regular? (make-card 1 2 5 10)) false)
(check-expect (card-regular? (make-card 0 1 0 9)) true)
(check-expect (card-regular? (make-card 1 2 2 2)) false)
(check-expect (card-regular? (make-card 2 3 4 1)) true)
;; (counter lst1 lst2 ctr) is a counter that consumes two lists and a beginning number ctr (must begin with 0)
;;    and produce a number that: if the coresponding elements in lst1 and lst2 are equal, ctr remain
;;    unchanged, if the coresponding elements in lst1 is greater than that in lst2, ctr is added by 1
;;    if the coresponding elements in lst2 is greater than that in lst1, ctr is sutracted by 1
;; counter: (listof Int) (listof Int) Num -> Num
;; Require: the length of two lists are the same; ctr must be 0 initially
;; Example
(check-expect (counter (cons 2(cons 3 empty)) (cons 0(cons 0 empty)) 0) 2)
(check-expect (counter (cons 7(cons 13 empty)) (cons 25(cons 2 empty)) 0) 0)
(check-expect (counter (cons 7(cons 2(cons 13 empty))) (cons 25(cons 3(cons 2 empty))) 0) -1)
(define (counter lst1 lst2 ctr)
  (cond [(empty? lst1) 0]
        [else
         (+ ctr (cond [(> (first lst1) (first lst2)) 1]
                      [(< (first lst1) (first lst2)) -1]
                      [else 0]) (counter (rest lst1)
                                         (rest lst2) ctr))]))

;; (card-battle card1 card2) takes two cards and return 'win if the card1 has majority of
;;    larger values among strength, speed, intelligence and charm;
;;    return 'lose if card2 has majority larger values among strength, speed, intelligence and charm;
;;    return 'draw if they have same amount of values greater than each other
;; card-battle: Card Card -> Sym
;; Requair: card1 and card2 are expected to be (make-card Int Int Int Int)
;; Example:
(check-expect (card-battle (make-card 1 3 3 3) (make-card 2 3 4 1)) 'lose)
(check-expect (card-battle (make-card 4 4 1 1) (make-card 3 3 2 2)) 'draw)
(check-expect (card-battle (make-card 5 3 2 0) (make-card 4 2 1 3)) 'win)

(define (card-battle card1 card2)
  (cond [(> (counter (card-to-list card1) (card-to-list card2) 0) 0) 'win]
        [(< (counter (card-to-list card1) (card-to-list card2) 0) 0) 'lose]
        [else 'draw]))

(check-expect (card-battle (make-card 5 3 3 3) (make-card 11 2 2 2)) 'win)
(check-expect (card-battle (make-card 7 5 2 2) (make-card 8 4 2 2)) 'draw)
(check-expect (card-battle (make-card 7 5 2 2) (make-card 8 5 3 3)) 'lose)
(check-expect (card-battle (make-card 0 0 0 0) (make-card 0 0 0 0)) 'draw)
(check-expect (card-battle (make-card 4 2 2 2) (make-card 5 2 3 3)) 'lose)
(check-expect (card-battle (make-card 1 1 1 1) (make-card 0 0 0 0)) 'win)

;; (card-select my-cards opponent-card) takes the my-cards (listof Card) and 
;;    the opponent-card (Card) and returen the first card from my-cards that 
;;    beats the opponent-cards; return false if none in my-cards beats
;;    the opponent-card
;; card-select: (listof Card) Card -> (anyof Card false)
;; Example:
(check-expect (card-select (cons (make-card 3 2 4 1)(cons (make-card 3 4 0 3) empty))
             (make-card 0 3 5 2)) (make-card 3 4 0 3))
(check-expect (card-select (cons (make-card 0 0 0 10)(cons (make-card 10 0 0 0)(cons (make-card 6 1 1 2) empty)))
             (make-card 1 3 5 1)) false)

(define (card-select my-cards opponent-card)
  (cond [(empty? my-cards) false]
        [else 
         (cond [(symbol=? (card-battle (first my-cards) opponent-card) 'win) (first my-cards)]
               [(or (symbol=? (card-battle (first my-cards) opponent-card) 'lose)
                    (symbol=? (card-battle (first my-cards) opponent-card) 'draw)) 
                (card-select (rest my-cards) opponent-card)])]))

;; Test:
(check-expect (card-select (cons (make-card 1 3 3 3)(cons (make-card 2 2 2 4)(cons (make-card 6 1 1 2) empty)))
             (make-card 2 3 4 1)) false)
(check-expect (card-select (cons (make-card 3 4 1 2)(cons (make-card 2 2 2 4)(cons (make-card 6 1 1 2) empty)))
             (make-card 2 3 4 1)) (make-card 3 4 1 2))
(check-expect (card-select (cons (make-card 1 3 3 3)(cons (make-card 2 2 2 4)(cons (make-card 6 1 1 2) empty)))
             (make-card 0 0 0 0)) (make-card 1 3 3 3))
(check-expect (card-select (cons (make-card 1 1 1 7)(cons (make-card 3 4 1 2) empty))
             (make-card 2 3 4 1)) (make-card 3 4 1 2))
(check-expect (card-select empty (make-card 10 0 0 0)) false)
(check-expect (card-select (cons (make-card 10 0 0 0) empty) (make-card 5 0 0 0)) (make-card 10 0 0 0))

;; (count-collector-cards some-cards) takes a list of Card (some-cards) and return the number of cards
;;    satisfy the collector cards requirement which is the sum of its strength, speed intellegence and charm
;;    is positive and greater than 3 except 10
;; count-collector-cards some-cards: (listof Card) -> Int
;; Example:
(check-expect (count-collector-cards
               (cons (make-card 1 0 0 1) empty)) 0)
(check-expect (count-collector-cards
              (cons (make-card 1 2 3 7)(cons (make-card 2 11 13 23) empty))) 2)

(define (count-collector-cards some-cards)
  (cond [(empty? some-cards) 0]
        [(and (> (sum-list (card-to-list (first some-cards))) 3)
              (not (= (sum-list (card-to-list (first some-cards))) 10)))
         (+ 1 (count-collector-cards (rest some-cards)))]
        [else (count-collector-cards (rest some-cards))]))

;; Test:
(check-expect (count-collector-cards
               (cons (make-card 1 3 3 3)(cons (make-card 2 2 2 4)(cons (make-card 6 1 1 2) empty)))) 0)
(check-expect (count-collector-cards
               (cons (make-card 2 5 7 13)(cons (make-card 2 2 2 4)(cons (make-card 6 1 1 2) empty)))) 1)
(check-expect (count-collector-cards
               (cons (make-card 2 5 7 13)(cons (make-card 99 999 9999 99999)(cons (make-card 6 1 3 2) empty)))) 3)
(check-expect (count-collector-cards
               (cons (make-card 0 0 0 0) empty)) 0)
(check-expect (count-collector-cards
               (cons (make-card 1 1 1 0) empty)) 0)
(check-expect (count-collector-cards
               (cons (make-card 1 2 3 4) empty)) 0)
(check-expect (count-collector-cards
               (cons (make-card 2 2 2 1) empty)) 1)
