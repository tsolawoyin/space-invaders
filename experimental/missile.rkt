;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname missile) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; mini program to design the functionality of missiles.

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define MISSILE-SPEED 10)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

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

