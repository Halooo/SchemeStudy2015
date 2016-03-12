;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rockets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 04, Problem 3
;; ***************************************************
;;

;;Constant:
(define g 9.8)


(define-struct stage (fuel-mass dry-mass thrust isp))
;; A Stage is a (make-stage Num Num Num Num)

;; The following information were created by CS 135 instructors and refer to
;;    CS 135 Home/Assignment/Assignment04/example-rockets.rkt
;;=== EXAMPLE NASA ROCKETS ===
(define silly-rocket (cons (make-stage 90 30 1629.25 3333)
                     (cons (make-stage 9   4  191.1  4500) empty)))

;; The following rockets were created by grabbing numbers from Wikipedia
;; and rounding them heavily.

;; An approximation of an Apollo mission configuration using a Saturn V rocket
;; The third stage's dry mass includes the lunar and service modules,
;; which can be viewed as stages in their own right.  
(define up-goer-five (cons (make-stage 2160   130   34020  2580) 
                     (cons (make-stage  456.1  40.1  4400  4130) 
	             (cons (make-stage  109.5  71  1000  4130) empty)))) 

;; An approximation of the lunar landers used in the Apollo missionns.
;; First stage lands on the Moon, second stage returns to lunar orbit.
;; The high TWR saves fuel in a vacuum, where drag is not an issue.
(define eagle       (cons  (make-stage 8.2  2.1  45  3050)         
       	            (cons  (make-stage 2.4  2.2  15  3050) empty)))

;; An approximation of the New Horizons mission.
;; The real one had boosters, which don't quite fit into our
;; "one stage after the other" conceptual model.
;; Stage 4 is the probe itself, which does not have a rocket 
;; engine, but does have maneuvering thrusters.
(define new-horizons (cons (make-stage 284.5  22.5  4152  3050)
                     (cons (make-stage 20.8    2.2  99.2  4400)
                     (cons (make-stage 2.1     0.1  66    2800)
                     (cons (make-stage 0.077   0.4 0.004  1800) empty)))))

;; === EXAMLE KSP ROCKET ===

;; loosely based on Kerbal X rocket from Kerbal Space Program
(define kerbal-x (cons (make-stage 12.5 4 2400 3030)
                 (cons (make-stage 12.5 4 2050 3030)
                 (cons (make-stage 12.5 4 1700 3030)
                 (cons (make-stage 48 13 1350 3030)
                 (cons (make-stage 8 11 250 3400) empty))))))

(define test-rocket-1 (cons (make-stage 79 53 1222.1 2333)
                     (cons (make-stage 24.5   5.5  97.8  3750) empty)))


;; (rocket-mass rocket) take a rocket and calculate the entire mass of the rocket in tonnes
;; rocket-mass: Rocket -> Num
;; Example:
(check-expect (rocket-mass test-rocket-1) 162)

(define (rocket-mass rocket)
  (cond [(empty? rocket) 0]
        [else (+ (stage-dry-mass (first rocket))
                 (stage-fuel-mass (first rocket))
                 (rocket-mass (rest rocket)))]))
;; Test:
(check-expect (rocket-mass empty) 0)
(check-expect (rocket-mass silly-rocket) 133)
(check-expect (rocket-mass up-goer-five) 2966.7)
(check-expect (rocket-mass eagle) 14.9)
(check-expect (rocket-mass new-horizons) 332.677)
(check-expect (rocket-mass kerbal-x) 129.5)



;; (rocket-twr rocket) takes a rocket and return a list of numbers and each element
;;    represents a minimum trust-to-weight ratio of the rocket while the stage is
;;    activated.
;; rocket-twr: Rocket -> (listof Num)
;; Example:
(check-expect (rocket-twr silly-rocket) (cons 1.25(cons 1.5 empty)))

(define (rocket-twr rocket)
  (cond [(empty? rocket) empty]
        [else (cons (/ (stage-thrust (first rocket)) (* (rocket-mass rocket) g))
                    (rocket-twr (rest rocket)))]))
;; Test:
(check-expect (rocket-twr empty) empty)
(check-expect (rocket-twr test-rocket-1) 
              (cons (/ 1222.1 (* (rocket-mass test-rocket-1) g))
                    (cons (/ 97.8 (* (+ 5.5 24.5) g)) empty)))
(check-expect (rocket-twr up-goer-five)
              (cons (/ 34020 (* (rocket-mass up-goer-five) g))
                    (cons (/ 4400 (* (- (rocket-mass up-goer-five) 2160 130) g))
                          (cons (/ 1000 (* 180.5 g)) empty))))
(check-expect (rocket-twr eagle)
              (cons (/ 45 (* (rocket-mass eagle) g))
                    (cons (/ 15 (* (- (rocket-mass eagle) 8.2 2.1) g)) empty)))
(check-expect (rocket-twr new-horizons)
              (cons (/ 4152 (* (rocket-mass new-horizons) g))
                    (cons (/ 99.2 (* (- (rocket-mass new-horizons) 284.5 22.5) g))
                          (cons (/ 66 (* (+ 2.2 0.477) g))
                                (cons (/ 0.004 (* 0.477 g)) empty)))))
(check-expect (rocket-twr kerbal-x)
              (cons (/ 2400 (* (rocket-mass kerbal-x) g))
                    (cons (/ 2050 (* (- (rocket-mass kerbal-x) 12.5 4) g))
                          (cons (/ 1700 (* (+ 12.5 4 48 13 11 8) g))
                                (cons (/ 1350 (* (+ 48 13 8 11) g))
                                      (cons (/ 250 (* 19 g)) empty))))))
;; (rocket-delta-v rocket) takes a Rocket (rocket) and calculate the total ∆v for
;;    the rocket, in m/s
;; rockt-delta-v: Rocket -> Num
(define (rocket-delta-v rocket)
  (cond [(empty? rocket) 0]
        [else (+ (* (stage-isp (first rocket)) (log (/ (rocket-mass rocket) (- (rocket-mass rocket) (stage-fuel-mass (first rocket)))))) (rocket-delta-v (rest rocket)))]))

(check-within (rocket-delta-v silly-rocket) 
              (+ (* 3333 (log (/ 133 43))) (* 4500 (log (/ 13 4)))) 1)
(check-within (rocket-delta-v up-goer-five) 
              (+ (* 2580 (log (/ (rocket-mass up-goer-five) (- (rocket-mass up-goer-five) 2160))))
                 (* 4130 (log (/ (+ 456.1 40.1 109.5 71) (+ 40.1 109.5 71))))
                 (* 4130 (log (/ 180.5 71)))) 1)
(check-within (rocket-delta-v eagle) 
              (+ (* 3050 (log (/ (+ 8.2 2.1 2.4 2.2) (+ 2.1 2.2 2.4)))) (* 3050 (log (/ 4.6 2.2)))) 1)
(check-within (rocket-delta-v new-horizons) 
              (+ (* 3050 (log (/ (rocket-mass new-horizons) (- (rocket-mass new-horizons) 284.5)))) 
                 (* 4400 (log (/ (+ 20.8 2.2 2.1 0.1 0.477) (+ 2.2 2.1 0.1 0.477)))) 
                 (* 2800 (log (/ 2.677 0.577))) 
                 (* 1800 (log (/ 0.477 0.4)))) 1)

