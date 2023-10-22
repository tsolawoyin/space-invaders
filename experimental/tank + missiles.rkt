;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |tank + missiles|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; mini design for tank + missiles.

;; loading important functions


;; TANK

;; let's load the materials to help us make the tank + missile world

;; CONSTANTS
(define WIDTH  300)
(define HEIGHT 500)

(define TANK-SPEED 2)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define MISSILE-SPEED 10)

(define MISSILE (ellipse 5 15 "solid" "red"))

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
#;
(define (main t)
  (big-bang t
    (on-tick next-tank)
    (to-draw render-tank)
    (on-key  handle-tank)))


;; Tank -> Tank
;; increase the tank current x-pos by speed while maintaining the inside the screen position of tank.
(check-expect (next-tank T0) (make-tank (+ (tank-x T0) (* TANK-SPEED (tank-dir T0))) (tank-dir T0))) ;moving right
(check-expect (next-tank T1) (make-tank (+ (tank-x T1) (* TANK-SPEED (tank-dir T1))) (tank-dir T1))) ;moving right
(check-expect (next-tank T2) (make-tank (+ (tank-x T2) (* TANK-SPEED (tank-dir T2))) (tank-dir T2))) ;moving left
(check-expect (next-tank T3) (make-tank (- WIDTH TANK-WIDTH/2) -1)) ;reach right ; turning back.
(check-expect (next-tank T4) (make-tank (+ 0 TANK-WIDTH/2) 1))      ;reach left
;(define (next-tank t) t) ;stub
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
(check-expect (render-tank T0) (place-image TANK
                                            (tank-x T0)
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
(check-expect (render-tank T4) (place-image TANK
                                            (tank-x T4)
                                            (- HEIGHT TANK-HEIGHT/2)
                                            BACKGROUND))
;(define (render-tank t) BACKGROUND) ;stub
(define (render-tank t)
  (place-image TANK
               (tank-x t)
               (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))

;(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
;(define T1 (make-tank 50 1))            ;going right
;(define T2 (make-tank 50 -1))           ;going left
;(define T3 (make-tank WIDTH 1)          ;reach right
;(define T4 (make-tank 0 -1)             ;reach left

;; Tank ke -> Tank
;; change the dir of tank if certain keys are pressed
;;  - move left if the left key is pressed
;;  - move right if the right key is pressed
(check-expect (handle-tank T0 "left")  (make-tank (tank-x T0) -1))
(check-expect (handle-tank T0 "right") (make-tank (tank-x T0) 1))
(check-expect (handle-tank T2 "left")  (make-tank (tank-x T2) -1))
(check-expect (handle-tank T2 "right") (make-tank (tank-x T2) 1))
(check-expect (handle-tank T2 "up")    (make-tank (tank-x T2) (tank-dir T2)))
;(define (handle-tank t k) t)
(define (handle-tank t ke)
  (cond [(key=? ke "left")  (make-tank (tank-x t) -1)]
        [(key=? ke "right") (make-tank (tank-x t) 1)]
        [else (make-tank (tank-x t) (tank-dir t))]))



;; data definition for missile
(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
;(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
;(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; function
#;
(define (main m)
  (big-bang m
    (on-tick next-missile)
    (to-draw render-missile)
    (on-key  shoot-missile)))

;; Missile -> Missile
;; decrease the height of missile by MISSILE-SPEED
(check-expect (next-missile M1) (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
;(define (next-missile m) m) ;stub
(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Missile -> Image
;; produce the image of missile
(check-expect (render-missile M1) (place-image MISSILE
                                               (missile-x M1)
                                               (missile-y M1)
                                               BACKGROUND))
;(define (render-missile m) BACKGROUND) ;stub
(define (render-missile m)
  (place-image MISSILE
               (missile-x m)
               (missile-y m)
               BACKGROUND))

;; Missile Ke -> Missile
;; produce missile when the space key is pressed
(check-expect (shoot-missile M1 " ") (make-missile (/ WIDTH 2) (- HEIGHT 10)))
(check-expect (shoot-missile M1 "a")  M1)
;(define (shoot-missile m ke) (make-missile 0 0)) ;stub
(define (shoot-missile m ke)
  (cond [(key=? ke " ") (make-missile (/ WIDTH 2) (- HEIGHT 10))]
        [else m]))


;; simple missile rendering complete.

;; now we need a lot of missiles.

;; actually it is list of missiles that we need but designing for one missile will make it easy to design for other ones as well.


;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. list of missiles
(define LOM0 empty)
(define LOM1 (cons (make-missile 100 230) empty))
(define LOM2 (cons (make-missile 150 200) LOM1))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

#;
(define (main2 lom)
  (big-bang lom
    (on-tick valid-missiles)
    (to-draw render-missiles)
    (on-key  shoot-missiles)))

;; ListOfMissile -> ListOfMissile
;; produce the next states of missiles
(check-expect (next-missiles LOM0) empty)
(check-expect (next-missiles LOM1) (cons (next-missile (make-missile 100 230))
                                         empty))
(check-expect (next-missiles LOM2) (cons (next-missile (make-missile 150 200))
                                         (cons (next-missile (make-missile 100 230))
                                               empty)))
;(define (next-missiles lom) lom) ;stub
(define (next-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (next-missile (first lom))
               (next-missiles (rest lom)))]))

;(define LOM0 empty)
;(define LOM1 (cons (make-missile 100 230) empty))
;(define LOM2 (cons (make-missile 150 200) LOM1))

;; ListOfMissile -> Image
;; render missile state
(check-expect (render-missiles LOM0) BACKGROUND)
(check-expect (render-missiles LOM1) (place-image MISSILE
                                                  100 230
                                                  BACKGROUND))
(check-expect (render-missiles LOM2) (place-image MISSILE
                                                  150 200
                                                  (place-image MISSILE
                                                               100 230
                                                               BACKGROUND)))
;(define (render-missiles lom) BACKGROUND)
(define (render-missiles lom)
  (cond [(empty? lom) BACKGROUND]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom)))]))


;(define LOM0 empty)
;(define LOM1 (cons (make-missile 100 230) empty))
;(define LOM2 (cons (make-missile 150 200) LOM1))

;; ListOfMissile Ke -> ListOfMissile
;; add new missile to list of missiles
(check-expect (shoot-missiles LOM0 " ") (cons (make-missile (/ WIDTH 2) (- HEIGHT 10)) empty))
(check-expect (shoot-missiles LOM0 "a") LOM0)
;(define (shoot-missiles lom ke) lom)

(define (shoot-missiles lom ke)
  (cond  [(key=? ke " ")
          (cons (make-missile (/ WIDTH 2) (- HEIGHT 10))
                lom)]
         [else lom]))

;; ListOfMissile -> ListOfMissile
;; filter missiles that are out of screen
(check-expect (filter-missiles (cons (make-missile 100 0) empty)) empty)
(check-expect (filter-missiles (cons (make-missile 100 100)
                                     (cons (make-missile 100 150)
                                           empty)))
              (cons (make-missile 100 100)
                    (cons (make-missile 100 150)
                          empty)))
(check-expect (filter-missiles (cons (make-missile 100 -20)
                                     empty))
              empty)
(check-expect (filter-missiles (cons (make-missile 100 20)
                                     (cons (make-missile 120 -230)
                                           (cons (make-missile 100 200)
                                                 empty))))
              (cons (make-missile 100 20)
                    (cons (make-missile 100 200)
                          empty)))
;; I think this test case is sufficient enough
;(define (filter-missiles lom) lom)

(define (filter-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (> (missile-y (first lom)) 0)
             (cons (first lom)
                   (filter-missiles (rest lom)))
             (filter-missiles (rest lom)))]))


;; ListOfMissile -> ListOfMissile
;; produces valid missile with their y-coordinate updated
(define (valid-missiles lom)
  (filter-missiles (next-missiles lom)))

;; makes sense.

;; We need only function to blend the missile with the tank, and that is handle key
;; the rest is just normal stuff

;; now a new level of complexity. See how the stuff is boomeraging?

(define-struct game (tank lom))
;; Game is (make-game Tank ListOfMissile)
;; interp. the game state
(define G0 (make-game (make-tank 100 1) empty)) ;tank with no missiles yet
(define G1 (make-game (make-tank 120 -1) (cons (make-missile 100 40) empty))) ;tank + one missile
(define G2 (make-game (make-tank 110 1) (cons (make-missile 100 40)
                                              (cons (make-missile 160 200)
                                                    (cons (make-missile 100 40)
                                                          empty)))))
#;
(define (fn-for-game g)
  (... (fn-for-tank (game-tank t))
       (fn-for-lom  (game-lom  g))))


(define (main g)
  (big-bang g
    (on-tick next-game)
    (to-draw render-game)
    (on-key  handle-game)))


;; Game -> Game
;; produce next state of game by updating the x-coordinate of tank and updating the y-coordinate of missiles
(check-expect (next-game G0) (make-game (next-tank (game-tank G0)) (next-missiles (game-lom G0))))
(check-expect (next-game G1) (make-game (next-tank (game-tank G1)) (next-missiles (game-lom G1))))
;(define (next-game g) g) ;stub

(define (next-game g)
  (make-game (next-tank       (game-tank g))
             (valid-missiles  (game-lom  g))))

;; Game -> Image
;; produce the image of the current game
(check-expect (render-game G0) (place-images (listofimage (game-lom G0))
                                             (listofpos   (game-lom G0))
                                             (place-image TANK
                                                          (tank-x (game-tank G0))
                                                          (- HEIGHT TANK-HEIGHT/2)
                                                          BACKGROUND)))
(check-expect (render-game G1) (place-images (listofimage (game-lom G1))
                                             (listofpos   (game-lom G1))
                                             (place-image TANK
                                                          (tank-x (game-tank G1))
                                                          (- HEIGHT TANK-HEIGHT/2)
                                                          BACKGROUND)))
(check-expect (render-game G2) (place-images (listofimage (game-lom G2))
                                             (listofpos   (game-lom G2))
                                             (place-image TANK
                                                          (tank-x (game-tank G2))
                                                          (- HEIGHT TANK-HEIGHT/2)
                                                          BACKGROUND)))
;(define (render-game g) BACKGROUND) ;stub
;<template from game>
(define (render-game g)
  (place-images (listofimage (game-lom g))
                (listofpos   (game-lom g))
                (place-image TANK
                             (tank-x (game-tank g))
                             (- HEIGHT TANK-HEIGHT/2)
                             BACKGROUND)))

;<helper function>
;; ListOfMissile -> ListOfPosn
;; produces the x and y coordinates of missiles
(check-expect (listofpos empty) empty)
(check-expect (listofpos (cons (make-missile 100 20) empty))
              (cons (make-posn 100 20)
                    empty))
;(define (listofpos lom) empty) ;stub

(define (listofpos lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-posn (missile-x (first lom))
                          (missile-y (first lom)))
               (listofpos (rest lom)))]))

;<helper function2>
;; ListOfImage -> ListOfImage
(check-expect (listofimage empty) empty)
(check-expect (listofimage (cons (make-missile 20 100) empty)) (list MISSILE))
(check-expect (listofimage (cons (make-missile 23 40)
                                 (cons (make-missile 45 98)
                                       empty)))
              (cons MISSILE
                    (cons MISSILE empty)))

;(define (listofimage lom) empty) ;stub
(define (listofimage lom)
  (cond [(empty? lom) empty]
        [else
         (cons MISSILE
               (listofimage (rest lom)))]))


;(define G0 (make-game (make-tank 100 1) empty)) ;tank with no missiles yet
;(define G1 (make-game (make-tank 120 -1) (cons (make-missile 100 40) empty))) ;tank + one missile
;(define G2 (make-game (make-tank 110 1) (cons (make-missile 100 40)
;                                              (cons (make-missile 160 200)
;                                                    (cons (make-missile 100 40)
;                                                          empty)))))


;; Game ke -> Game
;; insert new missile having an x-coordinate corresponding to the tank coord and a y-coord starting at the tip of tank
(check-expect (handle-game G0 " ") (make-game (handle-tank (game-tank G0) " ")
                                              (cons (make-missile (tank-x (game-tank G0))
                                                                  (- HEIGHT (image-height TANK)))
                                                    (game-lom G0))))
(check-expect (handle-game G1 " ") (make-game (handle-tank (game-tank G1) " ")
                                              (cons (make-missile (tank-x (game-tank G1))
                                                                  (- HEIGHT (image-height TANK)))
                                                    (game-lom G1))))
(check-expect (handle-game G1 "a") G1)
(check-expect (handle-game G1 "left") (make-game (handle-tank (game-tank G1) "left")
                                                 (game-lom G1)))
(check-expect (handle-game G1 "right") (make-game (handle-tank (game-tank G1) "right")
                                                 (game-lom G1)))
;(define (handle-game g ke) g)
(define (handle-game g ke)
  (cond [(key=? " " ke)
         (make-game (handle-tank (game-tank g) ke)
                    (cons (make-missile (tank-x (game-tank g))
                                        (- HEIGHT (image-height TANK)))
                          (game-lom g)))]
        [(or (key=? ke "left") (key=? ke "right"))
         (make-game (handle-tank (game-tank g) ke)
                    (game-lom g))]
        [else g]))

;; alright, the left and right key is not controlling the movement of this stuff. no wahala. more test cases coming up

;; now let's create functions that help create new missile

;; now makes sense. Olagbara. Things are really making sense
