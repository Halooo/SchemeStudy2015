;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quaternion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 03, Problem 5
;; ***************************************************
;;

(define-struct quaternion (cc ic jc kc))
;; a Quaternion is a (make-quaternion Num Num Num Num)

;; (quat-mult a b q1 q2) takes 2 non-zero numbers a,b and 2 quaternions q1 q2
;;    and produce the dot product of the 2 quaternions related to a,b
;; quat-mult: Num Num Quaternion Quaternion -> Quaternion

;; Example:
(check-expect (quat-mult -1 -1 (make-quaternion 2 3 0 0) (make-quaternion 5 6 0 0)) (make-quaternion -8 27 0 0))

(define (quat-mult a b q1 q2)
  (make-quaternion (+ (* (quaternion-cc q1) (quaternion-cc q2))
                       (* a (quaternion-ic q1) (quaternion-ic q2))
                       (* b (quaternion-jc q1) (quaternion-jc q2))
                       (* -1 a b (quaternion-kc q1) (quaternion-kc q2))) 
                    (+ (* -1 b (quaternion-jc q1) (quaternion-kc q2))
                       (* b (quaternion-kc q1) (quaternion-jc q2))
                       (* (quaternion-cc q1) (quaternion-ic q2))
                       (* (quaternion-ic q1) (quaternion-cc q2))) 
                    (+ (* -1 a (quaternion-kc q1) (quaternion-ic q2))
                       (* a (quaternion-ic q1) (quaternion-kc q2))
                       (* (quaternion-cc q1) (quaternion-jc q2))
                       (* (quaternion-jc q1) (quaternion-cc q2))) 
                    (+ (* -1 (quaternion-jc q1) (quaternion-ic q2))
                       (* (quaternion-ic q1) (quaternion-jc q2))
                       (* (quaternion-cc q1) (quaternion-kc q2))
                       (* (quaternion-kc q1) (quaternion-cc q2)))))

;; Test:
(check-expect (quat-mult 5 7 (make-quaternion 0 0 0 0) (make-quaternion 0 0 0 0)) (make-quaternion 0 0 0 0))
(check-expect (quat-mult 1 1 (make-quaternion 2 3 0 0) (make-quaternion 5 6 0 0)) (make-quaternion 28 27 0 0))
(check-expect (quat-mult 17 21 (make-quaternion 5 7 11 19) (make-quaternion 23 41 57 91)) (make-quaternion -599092 2088 -1876 840))
