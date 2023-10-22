;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |space invaders|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; CONSTANTS
(define WIDTH  300)
(define HEIGHT 500)

(define TANK-SPEED 2)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define MISSILE-SPEED 10)

(define MISSILE (ellipse 5 15 "solid" "red"))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)

(define INVADE-RATE 100)


(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))

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

;; Missile -> Missile
;; decrease the height of missile by MISSILE-SPEED
(check-expect (next-missile M1) (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
;(define (next-missile m) m) ;stub
(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; Missile Ke -> Missile
;; produce missile when the space key is pressed
(check-expect (shoot-missile M1 " ") (make-missile (/ WIDTH 2) (- HEIGHT 10)))
(check-expect (shoot-missile M1 "a")  M1)
;(define (shoot-missile m ke) (make-missile 0 0)) ;stub
(define (shoot-missile m ke)
  (cond [(key=? ke " ") (make-missile (/ WIDTH 2) (- HEIGHT 10))]
        [else m]))

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

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 3))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; Invader -> Invader
;; increases invader-x by INVADER-X-SPEED and increases invader-y by INVADER-Y-SPEED, if invader reaches left edge, the direction should change, likewise, the right edge as well
(check-expect (next-invader I1) (make-invader (+ (invader-x I1) (* (invader-dx I1) INVADER-X-SPEED))
                                              (+ (invader-y I1) INVADER-Y-SPEED)
                                              (invader-dx I1)))
(check-expect (next-invader (make-invader (+ 0 INVADER-WIDTH/2) ;;if reach left edge, change dir
                                          30 -1))
              (make-invader (+ 0 INVADER-WIDTH/2)
                            30 1))
(check-expect (next-invader (make-invader (- WIDTH INVADER-WIDTH/2) ;; if reach right edge, turn left
                                          30 1))
              (make-invader (- WIDTH INVADER-WIDTH/2)
                            30 -1))
;; pretty much all the tests we need. honestly.
;(define (next-invader i) i)
(define (next-invader i)
  (cond [(and (<= (invader-x i) (+ 0 INVADER-WIDTH/2))
              (< (invader-dx i) 0))
         (make-invader (invader-x i)
                       (invader-y i)
                       (* -1 (invader-dx i)))]
        [(and (>= (invader-x i) (- WIDTH INVADER-WIDTH/2))
              (> (invader-dx i) 0))
         (make-invader (invader-x i)
                       (invader-y i)
                       (* -1 (invader-dx i)))]
        [else (make-invader (+ (invader-x i) (* (invader-dx i) INVADER-X-SPEED))
                            (+ (invader-y i) INVADER-Y-SPEED)
                            (invader-dx i))]))

;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. list of invaders
(define LOI0 empty)
(define LOI1 (list I1))
(define LOI2 (list I1 I2))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; ListOfInvader -> ListOfInvader
;; produces the next state of invaders using the next-invader function
(check-expect (next-invaders empty) empty)
(check-expect (next-invaders LOI1) (list (next-invader I1)))
(check-expect (next-invaders LOI2) (list (next-invader I1) (next-invader I2)))
;(define (next-invaders loi) loi)
(define (next-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
               (next-invaders (rest loi)))]))
;<helper function>
;; ListOfInvader -> ListOfPosn
;; produces the x and y coordinates of invaders
(check-expect (listofpos empty) empty)
(check-expect (listofpos (cons (make-invader 100 20 1) empty))
              (cons (make-posn 100 20)
                    empty))
;(define (listofpos lom) empty) ;stub

(define (listofpos lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-posn (invader-x (first lom))
                          (invader-y (first lom)))
               (listofpos (rest lom)))]))

;<helper function2>
;; ListOfInvader -> ListOfImage
(check-expect (listofimage empty) empty)
(check-expect (listofimage (cons (make-invader 20 100 12) empty)) (list INVADER))
(check-expect (listofimage (cons (make-invader 23 40 1)
                                 (cons (make-invader 45 98 3)
                                       empty)))
              (cons INVADER
                    (cons INVADER empty)))

;(define (listofimage lom) empty) ;stub
(define (listofimage lom)
  (cond [(empty? lom) empty]
        [else
         (cons INVADER
               (listofimage (rest lom)))]))

(define (create-invader loi)
  (if (or (= (random (/ 10000 INVADE-RATE))
             1)
          (= (random (/ 10000 INVADE-RATE))
             0))
      (cons (make-invader (random WIDTH) 0 (+ (random 4) 1)) loi)
      loi))

(define (filter-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (if (<= (invader-y (first loi)) HEIGHT)
             (cons (first loi)
                   (filter-invaders (rest loi)))
             (listofimage (rest loi)))]))

(define (incoming-invaders loi)
  (filter-invaders (next-invaders (create-invader loi))))


;; ListOfMissile -> ListOfPosn
;; produces the x and y coordinates of missiles
(check-expect (listofpos2 empty) empty)
(check-expect (listofpos2 (cons (make-missile 100 20) empty))
              (cons (make-posn 100 20)
                    empty))
;(define (listofpos lom) empty) ;stub

(define (listofpos2 lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-posn (missile-x (first lom))
                          (missile-y (first lom)))
               (listofpos2 (rest lom)))]))

;; ListOfMissile -> ListOfImage
(check-expect (listofimage2 empty) empty)
(check-expect (listofimage2 (cons (make-missile 20 100) empty)) (list MISSILE))
(check-expect (listofimage2 (cons (make-missile 23 40)
                                  (cons (make-missile 45 98)
                                        empty)))
              (cons MISSILE
                    (cons MISSILE empty)))

;(define (listofimage2 lom) empty) ;stub
(define (listofimage2 lom)
  (cond [(empty? lom) empty]
        [else
         (cons MISSILE
               (listofimage2 (rest lom)))]))

;; ListOfInvader ListOfMissile -> ListOfMissile
;; remove invader from list if it collides with any missile
;; - criteria for collision:
;;    hit range is bound to the width and height of the invader
(check-expect (remove-invader (list (make-invader 50 50 1)
                                    (make-invader 40 200 -1))
                              (list (make-missile 40 40)))
              (list (make-invader 50 50 1)
                    (make-invader 40 200 -1)))
(check-expect (remove-invader (list (make-invader 50 50 1)
                                    (make-invader 40 200 -1))
                              (list (make-missile 40 50)))
              (list (make-invader 40 200 -1)))
(check-expect (remove-invader (list (make-invader 50 50 1)
                                    (make-invader 40 200 -1))
                              (list (make-missile 45 50)))
              (list (make-invader 40 200 -1)))
(check-expect (remove-invader (list (make-invader 50 50 1)
                                    (make-invader 40 200 -1))
                              (list (make-missile 55 50)))
              (list (make-invader 40 200 -1)))
(check-expect (remove-invader (list (make-invader 50 50 1)
                                    (make-invader 40 200 -1))
                              (list (make-missile 60 50)))
              (list (make-invader 40 200 -1)))
(check-expect (remove-invader (list (make-invader 50 50 1)
                                    (make-invader 40 200 -1))
                              (list (make-missile 50 42)))
              (list (make-invader 40 200 -1)))
(check-expect (remove-invader (list (make-invader 50 50 1)
                                    (make-invader 40 200 -1))
                              (list (make-missile 50 58)))
              (list (make-invader 40 200 -1)))
;(define (remove-invader loi lom) loi)

(define (remove-invader loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (not (collide? (first loi) lom))
             (cons (first loi)
                   (remove-invader (rest loi) lom))
             (remove-invader (rest loi) lom))]))

;; Invader ListOfMissile -> Boolean
;; produces true if invader collides with any of the missiles
(check-expect (collide? (make-invader 50 50 1)
                        (list (make-missile 50 50)))
              true)
(check-expect (collide? (make-invader 50 50 1)
                        (list (make-missile 30 90)))
              false)

;(define (collide? i lom) false) ;stub
(define (collide? i lom)
  (cond [(empty? lom) false]
        [else
         (if (collide-helper i (first lom))
             true
             (collide? i (rest lom)))]))

;; Invader Missile -> Boolean
;; produces true if invader and missile collides
(check-expect (collide-helper (make-invader 50 50 1)
                              (make-missile 50 50))
              true)
(check-expect (collide-helper (make-invader 50 50 1)
                              (make-missile 70 20))
              false)
;(define (collide-helper i m) true)

(define (collide-helper i m)
  (and (>= (missile-x m)
           (- (invader-x i) (/ (image-width INVADER) 2)))
       (<= (missile-x m)
           (+ (invader-x i) (/ (image-width INVADER) 2)))
       (>= (missile-y m)
           (- (invader-y i) (/ (image-height INVADER) 2)))
       (<= (missile-y m)
           (+ (invader-y i) (/ (image-height INVADER) 2)))))

;; the same function goes for removing missiles too. now maybe that's where we need some abstraction. but for now, we write everything out.

;; ListOfMissile ListOfInvader -> ListOfMissile
;; remove missile that have collided with a missile
(check-expect (remove-missile (list (make-missile 50 50)
                                    (make-missile 100 100))
                              (list (make-invader 50 50 1)))
              (list (make-missile 100 100)))
;(define (remove-missile lom loi) lom)
(define (remove-missile lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (not (collide2? (first lom) loi))
             (cons (first lom)
                   (remove-missile (rest lom) loi))
             (remove-missile (rest lom) loi))]))

(define (collide2? m loi)
  (cond [(empty? loi) false]
        [else
         (if (collide-helper (first loi) m)
             true
             (collide2? m (rest loi)))]))


(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define GAME (make-game empty empty (make-tank (/ WIDTH 2) 1)))

;; start game with (main GAME)
(define (main g)
  (big-bang g
    (on-tick next-game)
    (to-draw render-game)
    (on-key  handle-key)
    (stop-when reach-bottom? game-over)))

;; Game -> Game
;; produces the next state of game
(check-expect (next-game GAME)
              (make-game (incoming-invaders (remove-invader (game-invaders GAME) (game-missiles GAME)))
                         (valid-missiles    (remove-missile (game-missiles GAME) (game-invaders GAME)))
                         (next-tank (game-tank GAME))))
;(define (next-game g) g)
(define (next-game g)
  (make-game (incoming-invaders (remove-invader (game-invaders g) (game-missiles g)))
             (valid-missiles (remove-missile (game-missiles g) (game-invaders g)))
             (next-tank (game-tank g))))

;; Game -> Image
;; produce image of game
(check-expect (render-game GAME) (place-images (listofimage (game-invaders GAME))
                                               (listofpos   (game-invaders GAME))
                                               (place-images (listofimage2 (game-missiles GAME))
                                                             (listofpos2    (game-missiles GAME))
                                                             (place-image TANK
                                                                          (tank-x (game-tank GAME))
                                                                          (- HEIGHT TANK-HEIGHT/2)
                                                                          BACKGROUND))))
;(define (render-game g) BACKGROUND)
(define (render-game g)
  (place-images (listofimage (game-invaders g))
                (listofpos   (game-invaders g))
                (place-images (listofimage2 (game-missiles g))
                              (listofpos2    (game-missiles g))
                              (place-image TANK
                                           (tank-x (game-tank g))
                                           (- HEIGHT TANK-HEIGHT/2)
                                           BACKGROUND))))
;; Game ke -> Game
;; produce next game on key press
(define (handle-key g ke)
  (cond [(or (key=? ke " ")
             (key=? ke "left")
             (key=? ke "right"))
         (handle-game g ke)]
        [else g]))


(define (handle-game g ke)
  (cond [(key=? " " ke)
         (make-game (game-invaders g)
                    (cons (make-missile (tank-x (game-tank g))
                                        (- HEIGHT (image-height TANK)))
                          (game-missiles g))
                    (handle-tank (game-tank g) ke))]
        [(or (key=? ke "left") (key=? ke "right"))
         (make-game (game-invaders g)
                    (game-missiles g)
                    (handle-tank (game-tank g) ke))]))

;; everything works well and fine.

;; Game -> Boolean
;; produces true if any invader has a height greater than scrren
(define (reach-bottom? g)
  (reach-bottom-helper (game-invaders g)))

;; ListOfInvader -> Boolean
;; produces true if any of the invaders reaches a height greater than background
;(define (reach-bottom? loi) false)
(define (reach-bottom-helper loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) (- HEIGHT 10))
             true
             (reach-bottom-helper (rest loi)))]))

;; Game -> Image
;; produces last image when game is over
(define (game-over g)
  (overlay (text "GAME OVER 💀" 30 "red")
           (place-image (text "game by Olawoyin" 20 "black")
                        150 450
                        (render-game g))))