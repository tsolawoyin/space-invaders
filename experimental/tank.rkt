;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname tank) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; let's load the materials to help us make the tank world

(require 2htdp/image)
(require 2htdp/universe)

;; CONSTANTS
(define WIDTH  300)
(define HEIGHT 500)

(define TANK-SPEED 2)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2  (/ (image-width TANK) 2))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))             ;center going right
(define T1 (make-tank 50 1))                      ;going right
(define T2 (make-tank 50 -1))                     ;going left
(define T3 (make-tank (- WIDTH TANK-WIDTH/2) 1)) ;reach right
(define T4 (make-tank (+ 0 TANK-WIDTH/2) -1))     ;reach left
  
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


;; mini main program to design the functionality of tank

(define (main t)
  (big-bang t
    (on-tick next-tank)
    (to-draw render-tank)
    (on-key  handle-key)))



;(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
;(define T1 (make-tank 50 1))            ;going right
;(define T2 (make-tank 50 -1))           ;going left
;(define T3 (make-tank WIDTH 1)          ;reach right
;(define T4 (make-tank 0 -1)             ;reach left

;; Tank -> Tank
;; increase the tank current x-pos by speed while maintaining the inside the screen position of tank.
(check-expect (next-tank T0) (make-tank (+ (tank-x T0) (* TANK-SPEED (tank-dir T0))) (tank-dir T0))) ;moving right
(check-expect (next-tank T1) (make-tank (+ (tank-x T1) (* TANK-SPEED (tank-dir T1))) (tank-dir T1))) ;moving right
(check-expect (next-tank T2) (make-tank (+ (tank-x T2) (* TANK-SPEED (tank-dir T2))) (tank-dir T2))) ;moving left
(check-expect (next-tank T3) (make-tank (- WIDTH TANK-WIDTH/2) -1)) ;reach right ; turning back.
(check-expect (next-tank T4) (make-tank (+ 0 TANK-WIDTH/2) 1))      ;reach left
;; this test is kind of sufficient
;(define (next-tank t) t)
(define (next-tank t)
  (cond [(and (>= (tank-x t) (- WIDTH TANK-WIDTH/2))
              (= (tank-dir t) 1))
         (make-tank (- WIDTH TANK-WIDTH/2) -1)]
        [(and (<= (tank-x t) (+ 0 TANK-WIDTH/2))
              (= (tank-dir t) -1))
         (make-tank (+ 0 TANK-WIDTH/2) 1)]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))

;; Tank -> Image
;; produce the image of the tank given
;; !!!
(define (render-tank t) BACKGROUND)

;; Tank ke -> Tank
;; change the dir of tank if certain keys are pressed
;;  - move left if the left key is pressed
;;  - move right if the right key is pressed
;; !!!
(define (handle-key t) t)

;; we will also pack all the test cases and everything inside the game function.







