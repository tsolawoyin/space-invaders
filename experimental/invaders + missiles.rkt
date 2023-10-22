;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |invaders + missiles|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; working on missiles

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))

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

;; honestly, the function is pretty straight forward


(define (main i)
  (big-bang i
    (on-tick next-invader)
    (to-draw render-i)))

;; there is no key event relating to invaders
;(define I1 (make-invader 150 100 12))           ;not landed, moving right
;(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
;(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

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
         

;; Invader -> Image
;; render image of invader
(check-expect (render-i (make-invader 20 30 2)) (place-image INVADER
                                                             20 30
                                                             BACKGROUND))
;(define (render-i i) BACKGROUND)
(define (render-i i)
  (place-image INVADER
               (invader-x i)
               (invader-y i)
               BACKGROUND))

;; now do you see how easy it was. told ya. can't wait to upload this game on github. honestly.

;; now let's design loi


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

;; let's design another main function that will help with the display of invaders. And two, all randomness must be completed here so there is still a lot of work to do.

(define (main2 loi)
  (big-bang loi
    (on-tick incoming-invaders)
    (to-draw render-invaders)))

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

;; ListOfInvader -> Image
;; render images of invaders
(check-expect (render-invaders LOI0) (place-images (listofimage LOI0)
                                                   (listofpos   LOI0)
                                                   BACKGROUND))
(check-expect (render-invaders LOI2) (place-images (listofimage LOI2)
                                                   (listofpos   LOI2)
                                                   BACKGROUND))
;(define (render-invaders loi) BACKGROUND)
(define (render-invaders loi)
  (place-images (listofimage loi)
                (listofpos   loi)
                BACKGROUND))
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

;; works. no qualms. now, we need to generate random invaders at the top of screen

;; since it's random, we can't say for sure how the test case will work

(define (create-invader loi)
  (if (or (= (random (/ 10000 INVADE-RATE))
             1)
          (= (random (/ 10000 INVADE-RATE))
             0))
      (cons (make-invader (random WIDTH) 0 (random 5)) loi)
      loi))

(define (filter-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (if (< (invader-y (first loi)) HEIGHT)
             (cons (first loi)
                   (filter-invaders (rest loi)))
             (listofimage (rest loi)))]))

(define (incoming-invaders loi)
  (filter-invaders (next-invaders (create-invader loi))))

;; what else do we need for this to function properly? that's kind of all.

(define MISSILE-SPEED 10)

(define MISSILE (ellipse 5 15 "solid" "red"))

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

;; so let's design the prototype of the function here

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

;; ListOfImage -> ListOfImage
(check-expect (listofimage2 empty) empty)
(check-expect (listofimage2 (cons (make-missile 20 100) empty)) (list MISSILE))
(check-expect (listofimage2 (cons (make-missile 23 40)
                                  (cons (make-missile 45 98)
                                        empty)))
              (cons MISSILE
                    (cons MISSILE empty)))

;(define (listofimage lom) empty) ;stub
(define (listofimage2 lom)
  (cond [(empty? lom) empty]
        [else
         (cons MISSILE
               (listofimage2 (rest lom)))]))

;; loading files to help complete invader + missile function


(define-struct game (loi lom))
;; Game is (make-game ListOfInvader ListOfMissile)
;; interp. loi is the active list of invaders in the game
;;         lom is the active list of missiles in the game
(define G0 (make-game empty empty))
(define G1 (make-game (list (make-invader 20 20 1))
                      (list (make-missile 30 40))))
#;
(define (fn-for-game g)
  (... (fn-for-loi (game-loi g))
       (fn-for-lom (game-lom g))))


(define (main-fun g)
  (big-bang g
    (on-tick next-game)
    (to-draw render-game)
    (on-key  handle-game)))


;; Game -> Game
;; produce next state of game

(check-expect (next-game G0) (make-game empty empty))
(check-expect (next-game G1) (make-game (incoming-invaders (remove-invader (game-loi G1) (game-lom G1)))
                                        (valid-missiles (remove-missile (game-lom G1) (game-loi G1)))))
;(define (next-game g) g)

(define (next-game g)
  (make-game (incoming-invaders (remove-invader (game-loi g) (game-lom g)))
             (valid-missiles (remove-missile (game-lom g) (game-loi g)))))

;; Game -> Image
;; render game image
(check-expect (render-game G1) (place-images (listofimage (game-loi G1))
                                             (listofpos   (game-loi G1))
                                             (place-images (listofimage2 (game-lom G1))
                                                          (listofpos2   (game-lom G1))
                                                          BACKGROUND)))
;(define (render-game g) BACKGROUND)
(define (render-game g)
  (place-images (listofimage (game-loi g))
                (listofpos   (game-loi g))
                (place-images (listofimage2 (game-lom g))
                              (listofpos2   (game-lom g))
                              BACKGROUND)))

;; Game Ke -> Game
;; ...???
;; !!!
(define (handle-game g ke)
  (cond [(key=? ke " ") (make-game (game-loi g)
                                   (shoot-missiles (game-lom g) ke))]
        [else g]))



;; the filter function is absolutely perfect in its name,

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
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; what I did here is risky, but life is risky normally






