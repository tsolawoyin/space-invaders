;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


