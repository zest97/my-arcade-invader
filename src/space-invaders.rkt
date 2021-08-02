;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define INVADER-RIGHT-EDGE (- WIDTH INVADER-WIDTH/2))
(define INVADER-LEFT-EDGE INVADER-WIDTH/2)

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; Data Definitions:

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



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; =================
;; Functions:

;; G -> G
;; start the world with ...
;; 
(define (main g)
  (big-bang g                 ; G
    (on-tick   tock)           ; G -> G
    (to-draw   render)         ; G -> Image
    (stop-when exit-on)        ; G -> Boolean
    (on-key    handle-key)))   ; G KeyEvent -> G


;; G -> G
;; produce the next game with updated position and filtering hitted invaders and missiles
; (define (tock g) g) ; stub

(define (tock g)
  (filter-hit (tock-game g)))


;; G -> G
;; produce the new game by filtering hitted missiles and invaders

; (define (filter-hit g) g) ; stub

(define (filter-hit g)
  (make-game (filter-invaders (game-invaders g) (game-missiles g))
             (filter-missiles (game-missiles g) (game-invaders g))
             (game-tank g)))


;; listOf Invaders, listOf Missiles -> listOf Invaders
;; produce the next listOf invaders by filtering the hitted invaders

(check-expect (filter-invaders empty empty) empty)
(check-expect (filter-invaders (list (make-invader 165 200 1)
                                     (make-invader 30 350 -1)
                                     (make-invader 200 400 1))
                               (list (make-missile 28 353)
                                     (make-missile 80 29)))
              (list (make-invader 165 200 1)
                    (make-invader 200 400 1)))
(check-expect (filter-invaders (list (make-invader 165 200 1)
                                     (make-invader 30 350 -1)
                                     (make-invader 200 400 1))
                               (list (make-missile 80 29)))
              (list (make-invader 165 200 1)
                    (make-invader 30 350 -1)
                    (make-invader 200 400 1)))

; (define (filter-invaders loi lom) loi) ; stub

(define (filter-invaders loi lom)
  (cond [(empty? loi) loi]
        [else
         (cond [(is-invader-hit? (first loi) lom)
                (filter-invaders (rest loi) lom)]
               [else
                (cons (make-invader (invader-x (first loi))
                                    (invader-y (first loi))
                                    (invader-dx (first loi)))
                      (filter-invaders (rest loi) lom))
                ])
         ]))


;; Invader, listOf Missiles -> Boolean
;; produce true if invader is in hit-range of each missile

(check-expect (is-invader-hit? (make-invader 75 200 1) empty) false)
(check-expect (is-invader-hit? (make-invader 75 200 1)
                               (list (make-missile 160 30)
                                     (make-missile 240 300))) false)
(check-expect (is-invader-hit? (make-invader 75 200 1)
                               (list (make-missile 160 30)
                                     (make-missile 70 207))) true)

; (define (is-invader-hit? i lom) false) ; stub

(define (is-invader-hit? i lom)
  (cond [(empty? lom) false]
        [else
         (cond [(and (and (>= (missile-y (first lom)) (invader-y i))
                          (<= (- (missile-y (first lom)) (invader-y i)) HIT-RANGE))
                     (and (> (invader-x i) (missile-x (first lom)))
                          (<= (- (invader-x i) (missile-x (first lom))) HIT-RANGE)))
                true]
               [(and (and (>= (missile-y (first lom)) (invader-y i))
                          (<= (- (missile-y (first lom)) (invader-y i)) HIT-RANGE))
                     (and (> (missile-x (first lom)) (invader-x i))
                          (<= (- (missile-x (first lom)) (invader-x i)) HIT-RANGE)))
                true]
               [else
                (is-invader-hit? i (rest lom))])
         ]))


;; listOf Missiles, listOf Invaders -> listOf Missiles
;; produce the next listOf missiles by filtering the hitted missiles

(check-expect (filter-missiles empty empty) empty)
(check-expect (filter-missiles (list (make-missile 28 353)
                                     (make-missile 80 29))
                               (list (make-invader 165 200 1)
                                     (make-invader 30 350 -1)
                                     (make-invader 200 400 1)))
              (list (make-missile 80 29)))
(check-expect (filter-missiles (list (make-missile 80 29))
                               (list (make-invader 165 200 1)
                                     (make-invader 30 350 -1)
                                     (make-invader 200 400 1)))
              (list (make-missile 80 29)))

; (define (filter-missiles lom loi) lom) ; stub

(define (filter-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (cond [(is-missile-hit? (first lom) loi)
                (filter-missiles (rest lom) loi)]
               [else
                (cons (make-missile (missile-x (first lom))
                                    (missile-y (first lom)))
                      (filter-missiles (rest lom) loi))
                ])
         ]))


;; Missile, listOf Invaders -> Boolean
;; produce true if missile is in hit-range to one of invader in the list

(check-expect (is-missile-hit? (make-missile 80 29) empty) false)
(check-expect (is-missile-hit? (make-missile 80 29)
                               (list (make-invader 165 200 1)
                                     (make-invader 30 350 -1)
                                     (make-invader 200 400 1))) false)
(check-expect (is-missile-hit? (make-missile 28 353)
                               (list (make-invader 165 200 1)
                                     (make-invader 30 350 -1)
                                     (make-invader 200 400 1))) true)

; (define (is-missile-hit? m loi) false) ; stub

(define (is-missile-hit? m loi)
  (cond [(empty? loi) false]
        [else
         (cond [(and (and (>= (missile-y m) (invader-y (first loi)))
                          (<= (- (missile-y m) (invader-y (first loi))) HIT-RANGE))
                     (and (> (invader-x (first loi)) (missile-x m))
                          (<= (- (invader-x (first loi)) (missile-x m)) HIT-RANGE)))
                true]
               [(and (and (>= (missile-y m) (invader-y (first loi)))
                          (<= (- (missile-y m) (invader-y (first loi))) HIT-RANGE))
                     (and (> (missile-x m) (invader-x (first loi)))
                          (<= (- (missile-x m) (invader-x (first loi))) HIT-RANGE)))
                true]
               [else
                (is-missile-hit? m (rest loi))])
         ]))


;; G -> G
;; produce the next game by advancing the list of invaders, missiles and tank

(define (tock-game g)
  (make-game (spawn-random-invaders (tock-invaders (game-invaders g)))
             (tock-missiles (game-missiles g))
             (tock-tank (game-tank g))))


;; listOf Invaders -> listOf Invaders
;; produce new list of invaders with update x and y pos on each of invader

(check-expect (tock-invaders (list (make-invader 165 200 1)
                                     (make-invader 30 350 -1)
                                     (make-invader 200 400 1)))
              (list (make-invader (+ 165 (* INVADER-Y-SPEED 1)) (+ 200 INVADER-Y-SPEED) 1)
                                     (make-invader (+ 30 (* INVADER-Y-SPEED -1)) (+ 350 INVADER-Y-SPEED) -1)
                                     (make-invader (+ 200 (* INVADER-Y-SPEED 1)) (+ 400 INVADER-Y-SPEED) 1)))

; (define (tock-invaders loi) loi) ; stub

(define (tock-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader (first loi))
               (tock-invaders (rest loi)))
         ]))


;; Invader -> Invader
;; produce invader at the new position with shifted x and y pos

(check-expect (advance-invader (make-invader 0 180 -1))
              (make-invader INVADER-LEFT-EDGE (+ 180 INVADER-Y-SPEED) 1))
(check-expect (advance-invader (make-invader WIDTH 180 1))
              (make-invader INVADER-RIGHT-EDGE (+ 180 INVADER-Y-SPEED) -1))
(check-expect (advance-invader (make-invader 40 180 1))
              (make-invader (+ 40 (* INVADER-Y-SPEED 1)) (+ 180 INVADER-Y-SPEED) 1))

; (define (advance-invader i) i); stub

(define (advance-invader i)
  (cond [(< (+ (invader-x i) (invader-dx i)) INVADER-LEFT-EDGE)
         (make-invader INVADER-LEFT-EDGE
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- (invader-dx i)))]
        [(>= (+ (invader-x i) (invader-dx i))
             INVADER-RIGHT-EDGE)
         (make-invader INVADER-RIGHT-EDGE
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (* INVADER-Y-SPEED (invader-dx i)))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))
         ]))


;; listOf Invaders -> listOf Invaders
;; produce listOf invaders with randomly added invader over INVADE-RATE

; (define (spawn-random-invaders loi) loi) ; stub

(define (spawn-random-invaders loi)
  (cond[(< (random 5000) INVADE-RATE)
        (cons (make-invader (random WIDTH) 0 1) loi)]
       [else loi]))


;; listOf Missile -> listOf Missile
;; produce new list with update y pos on each of missile

; (define (tock-missiles lom) lom) ; stub

(define (tock-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cond [(is-missile-over-box? (first lom))
                (tock-missiles (rest lom))]
               [else
                (append (list (advance-missile (first lom)))
                        (tock-missiles (rest lom)))])
         ]))


;; Missile -> Boolean
;; produce true if missile is out of the game box; else false

(check-expect (is-missile-over-box? (make-missile 200 0)) true)
(check-expect (is-missile-over-box? (make-missile 50 150)) false)

; (define (is-missile-over-box? m) false) ; stub

(define (is-missile-over-box? m)
  (< (- (missile-y m) MISSILE-SPEED) 0))


;; Missile -> Missile
;; reduce y pos of missile till 0 per MISSILE-SPEED on tick

(check-expect (advance-missile (make-missile 200 300)) (make-missile 200 (- 300 MISSILE-SPEED)))
(check-expect (advance-missile (make-missile 50 150)) (make-missile 50 (- 150 MISSILE-SPEED)))

; (define (advance-missile m) m) ; stub

(define (advance-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))


;; TANK -> TANK
;; produce tank at the new x pos per speed with the given dir

(check-expect (tock-tank (make-tank (/ WIDTH 2) 0)) (make-tank (/ WIDTH 2) 0))
(check-expect (tock-tank (make-tank 50 1))
              (make-tank (+ 50 (* TANK-SPEED 1)) 1))
(check-expect (tock-tank (make-tank 1 -1))
              (make-tank TANK-WIDTH/2 -1))
(check-expect (tock-tank (make-tank WIDTH 1))
              (make-tank (- WIDTH TANK-WIDTH/2) 1))

; (define (tock-tank t) t) ; stub

(define (tock-tank t)
  (cond [(< (+ (tank-x t)
               (* TANK-SPEED (tank-dir t)))
            TANK-WIDTH/2)
         (make-tank TANK-WIDTH/2 (tank-dir t))]
        [(> (+ (tank-x t)
               (* TANK-SPEED (tank-dir t)))
            (- WIDTH TANK-WIDTH/2))
         (make-tank (- WIDTH TANK-WIDTH/2)
                    (tank-dir t))]
        [else
         (make-tank (+ (tank-x t)
                       (* TANK-SPEED (tank-dir t)))
                    (tank-dir t))
         ]))


;; G -> Image
;; render next game with advancing invaders, missiles and tank

; (define (render g) (square 0 "solid" "white")); stub

(define (render g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g)))))


;; listOf Invaders, Image -> Image
;; produce images of invader placing on the given background image

(define (render-invaders loi bg)
  (cond [(empty? loi) bg]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) bg))]))


;; listOf Missiles, Image -> Image
;; produce images of missile placing on the given background image

(define (render-missiles lom bg)
  (cond [(empty? lom) bg]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) bg))]))


;; Tank -> Image
;; produce tank image on the BACKGROUND with the given x pos

(check-expect (render-tank (make-tank 50 1))
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; G -> Boolean
;; produce true if the game should be ended; else false

(define (exit-on g)
  (cond [(hit-ground? (game-invaders g)) true]
        [else false]))


;; listOf Invaders -> Boolean
;; produce true if one of invader in the list hit the ground of the game; else false

(check-expect (hit-ground? (list (make-invader 208 114 1) (make-invader 1 351 -1))) false)
(check-expect (hit-ground? (list (make-invader 208 HEIGHT 1) (make-invader 2 501 -1))) true)

(define (hit-ground? loi)
  (cond [(empty? loi) false]
        [else
         (cond [(>= (invader-y (first loi)) HEIGHT) true]
               [else
                (hit-ground? (rest loi))])]))


;; G KeyEvent -> G
;; render new game on each key press performing missile or tank moving

; (define (handle-key g ke) g) ; stub

(define (handle-key g ke)
  (cond [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (flick-tank-dir (game-tank g) 1))]
        [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (flick-tank-dir (game-tank g) -1))]
        [(key=? ke " ")
         (make-game (game-invaders g)
                    (launch-missile (game-missiles g) (game-tank g))
                    (game-tank g))]
        [else
         (make-game (game-invaders g)
                    (game-missiles g)
                    (game-tank g))]))


;; listof Missile, Tank -> listof Missile
;; produce new missile in the given list at the given x y coordinate

(check-expect (launch-missile (list (make-missile 230 150)) (make-tank 285 1))
              (list (make-missile 230 150)
                    (make-missile 285 (- HEIGHT (image-height TANK)))))

; (define (launch-missile lom) lom) ;stub

(define (launch-missile lom t)
  (cond [(empty? lom)
         (list (make-missile (tank-x t)
                             (- HEIGHT (image-height TANK))))]
        [else
         (append lom
                 (list (make-missile (tank-x t)
                                     (- HEIGHT (image-height TANK)))))]))


;; TANK, Number(1,-1) -> TANK
;; flick the direction of the tank

(check-expect (flick-tank-dir (make-tank 50 1) -1)
              (make-tank 50 -1))
(check-expect (flick-tank-dir (make-tank 50 -1) 1)
              (make-tank 50 1))

; (define (flick-tank-dir t dir) t) ; stub

(define (flick-tank-dir t dir)
  (make-tank (tank-x t)
             dir))


;; Run the game
(define INIT (make-game empty
                        empty
                        (make-tank (/ WIDTH 2) 0)))

(main INIT)