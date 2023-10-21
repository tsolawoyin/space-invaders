;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname note) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; NOTE ON THE PROGRESS OF THE PROJECT
;; Started on Saturday 21-oct-2023 <for the second time>
;; Doing the project to have a lot of htc concepts packed into my brain

;; Be patient, will surely take days to complete

;; We work on it piece by piece.

;; TO DO
;; - tank ✅ 21-oct-23, 6:45
;; - missiles ✅ 21-oct-23, 21:07 
;; - tank + missiles
;; - UFOs
;; - UFOs + missiles
;; - tank + missiles + UFOs = game

;; DOMAIN ANALYSIS
;; CONSTANTS
;; - WIDTH
;; - HEIGHT
;; - BACKGROUND
;; - MISSILE
;; - TANK
;; - UFO
;; - TANK-X-SPEED
;; - ENEMY-X-SPEED
;; - ENEMY-Y-SPEED
;; - MISSILE-SPEED
;; - ENEMY-ANGLE

;; CHANGING:
;; TANK-X: TANK-X-COORDINATE
;; ENEMY-X: ENEMY-X-COORDINATE
;; ENEMY-Y: ENEMY-Y-COORDINATE
;; MISSILE-Y: MISSILE-Y-COORDINATE
;; number of enemy on screen

;; BIG-BANG OPTIONS
;; on-tick
;; to-draw
;; on-key
;; stop-when (last-world, last-picture)