;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define BACKGROUND-COLOR "white")
(define TANK-COLOR-TOP "black")
(define TANK-COLOR-MIDDLE "black")
(define TANK-COLOR-BOTTOM "khaki")
(define INVADER-COLOR-TOP "blue")
(define INVADER-COLOR-BOTTOM "lavender")

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 6)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10) ; TODO: interp.

(define INVADE-RATE 100) ; TODO: interp.

(define SCENE (empty-scene WIDTH HEIGHT BACKGROUND-COLOR))

(define INVADER
  (underlay/xy (ellipse 10 15 "solid" INVADER-COLOR-TOP)              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   INVADER-COLOR-BOTTOM)))            ;saucer
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define INVADER-RIGHT-EDGE (- WIDTH INVADER-WIDTH/2))
(define INVADER-LEFT-EDGE INVADER-WIDTH/2)
  
(define TANK
  (overlay/offset (overlay (ellipse 28 8 "solid" TANK-COLOR-BOTTOM)       ;tread center
                       (ellipse 30 10 "solid" "black"))     ;tread outline
              0 -10
              (above
               (overlay                               ;gun
                (rectangle 5 10 "solid" TANK-COLOR-TOP)
                (rectangle 7 12 "solid" "black"))
                (rectangle 20 10 "solid" TANK-COLOR-MIDDLE))))   ;main body
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define TANK-LEFT-EDGE TANK-WIDTH/2)
(define TANK-RIGHT-EDGE (- WIDTH TANK-WIDTH/2))

(define MISSILE (ellipse 5 15 "solid" "darkred"))
(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))
(define MAX-MISSILES 5)



;; Data Definitions:

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank (/ WIDTH 2) 1))            ;going right
(define T2 (make-tank (/ WIDTH 2) -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y - INVADER-HEIGHT/2) in screen coordinates
;;         the invader moves along x by dx pixels per clock tick

(define I1 (make-invader (/ WIDTH 2) (/ HEIGHT 2) 2))                       ; not landed, moving right
(define I4 (make-invader (/ WIDTH 2) (/ HEIGHT 3) 4))                       ; not landed, moving right
(define I2 (make-invader (/ WIDTH 2) (- HEIGHT INVADER-HEIGHT/2) -2))       ; exactly landed, moving left
(define I3 (make-invader (/ WIDTH 2) (+ HEIGHT INVADER-HEIGHT/2) 2))        ; landed, out of the scene, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile (/ WIDTH 2) (/ HEIGHT 4)))              ; not hit I1
(define M4 (make-missile (/ WIDTH 2) (/ HEIGHT 5)))
(define M2 (make-missile (invader-x I1) (invader-y I1)))         ; exactly hit I1
(define M3 (make-missile (+ (invader-x I1) 5) (invader-y I1)))   ; hit I1 to the right

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define-struct game (invaders missiles tank))
;; Game is (make-game  ListtOfInvader ListOfMissile Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ListOfInvader is one of:
;; - '()
;; - (cons Invader ListOfInvader)
;; interp. List of invaders in the game
(define LOI0 '())
(define LOI1 (cons I1 (cons I4 '())))

#;
(define (fn-for-loi loi)
  (cond
    ((empty? loi) '())
    (else
     (... (fn-for-invader (first loi))
          (fn-for-loi (rest loi))))))

;; ListOfMissile is one of:
;; - '()
;; - (cons Missile ListOfMissile)
;; interp. List of fire missiles
(define LOM0 '())
(define LOM1 (cons M1 (cons M4 '())))

;; Functions:

;; Game -> Game
;; Runs game; start with (main (make-game '() '() T0))
(define (main game)
  (big-bang game
    (on-tick update-game) ; Game -> Game
    (on-key handle-keys)  ; Game KeyEvent -> Game
    (to-draw render)    ; Game -> Image
    (stop-when game-end?))) ; Game -> Boolean

;; Game -> Boolean
;; Return #true if any invader reaches bottom
;(define (game-end? game) #false) ; stub
(check-expect (game-end? G0) #false)
(define TEG0 (make-game (list (make-invader (/ WIDTH 2) HEIGHT 1))
                                    '()
                                    (make-tank (/ WIDTH 2) 1)))
(check-expect (game-end? TEG0) #true)

(define (game-end? game)
  (invader-reached-bottom? (game-invaders game)))

;; ListOfInvader -> Boolean
;; Return #true if any invader reaches the bottom
(define (invader-reached-bottom? loi)
  (cond
    ((empty? loi) #false)
    (else
     (if (>= (invader-y (first loi)) HEIGHT)
         #true
         (invader-reached-bottom? (rest loi))))))



;; Game -> Image
;; Render game on the screen
;(define (render game) SCENE) ; stub
(check-expect (render G2)
              (render-tank (game-tank G2)
                           (render-invaders (game-invaders G2)
                                            (render-missiles (game-missiles G2) SCENE))))
(define (render game)
  (render-tank (game-tank game)
               (render-invaders (game-invaders game)
                                (render-missiles (game-missiles game) SCENE))))

;; Tank Image -> Image
;; Render tank on the image
;(define (render-tank tank) SCENE) ; stub
(check-expect (render-tank T0 SCENE)
              (place-image TANK
                           (tank-x T0) TANK-Y SCENE))
(define (render-tank tank img)
  (place-image TANK
               (tank-x tank) TANK-Y img))
               
;; ListOfInvader Image -> Image
;; Render invaders on the image
;(define (render-invaders loi) SCENE) ; stub
(check-expect (render-invaders '() SCENE) SCENE)
(check-expect (render-invaders LOI1 SCENE)
              (render-invader I1
                              (render-invader I4 SCENE)))


(define (render-invaders loi img)
  (cond
    ((empty? loi) img)
    (else
     (render-invader (first loi)
                     (render-invaders (rest loi) img)))))

;; Invader -> Image
;; Render one invader
;(define (render-invader inv) SCENE) ; stub
(check-expect (render-invader I1 SCENE)
              (place-image
               INVADER
               (invader-x I1) (invader-y I1)
               SCENE))

(define (render-invader inv img)
  (place-image
   INVADER
   (invader-x inv) (invader-y inv)
               img))
  
;; ListOfMissile Image -> Image
;; Render missiles on the image
;(define (render-missiles lom) SCENE) ; stub
(check-expect (render-missiles '() SCENE) SCENE)
(check-expect (render-missiles LOM1 SCENE)
              (render-missile M1
                              (render-missile M4 SCENE)))
(define (render-missiles lom img)
  (cond
    ((empty? lom) img)
    (else
     (render-missile (first lom)
                     (render-missiles (rest lom) img)))))


;; Missile Image -> Image
;; Render single missile
(check-expect (render-missile M1 SCENE)
              (place-image
               MISSILE
               (missile-x M1) (missile-y M1)
               SCENE))

(define (render-missile msl img)
  (place-image
   MISSILE
   (missile-x msl) (missile-y msl)
   img))


;; Game KeyEvent -> Game
;; Handle keys in game
;; !!!
;(define (handle-keys game ke) game) ; stub
(check-expect (handle-keys G0 "left")
              (tank-change-direction G0 -1))
(check-expect (handle-keys G0 "right")
              (tank-change-direction G0 1))
(check-expect (handle-keys G0 "a") G0)

(define (handle-keys game ke)
  (cond
    ((string=? ke "left") (tank-change-direction game -1))
    ((string=? ke "right") (tank-change-direction game 1))
    ((string=? ke " ") (tank-fire-missile game))
    (else game)))
     
;; Game Direction -> Game
;; Change direction of a tank
;(define (tank-change-direction game dir) game) ; stub
(check-expect (tank-change-direction G0 -1)
              (make-game (game-invaders G0) (game-missiles G0)
                         (make-tank (tank-x (game-tank G0)) -1)))
(define (tank-change-direction game dir)
  (make-game (game-invaders game) (game-missiles game)
             (make-tank (tank-x (game-tank game)) dir)))

;; Game -> Game
;; Fire missile from tank at current position
;(define (tank-fire-missile game) game) ; stub
(check-expect (tank-fire-missile G0)
              (make-game (game-invaders G0)
                         (cons (fire-missile (game-tank G0))
                               (game-missiles G0))
                         (game-tank G0)))

(define (tank-fire-missile game)
              (make-game (game-invaders game)
                         (if (< (length (game-missiles game)) MAX-MISSILES)
                             (cons (fire-missile (game-tank game))
                                   (game-missiles game))
                             (game-missiles game))
                         (game-tank game)))


;; Tank -> Missile
;; Return a new missile shoot from tank
(define (fire-missile tank)
  (make-missile (tank-x tank) TANK-Y))
(check-expect (fire-missile T0)
              (make-missile (tank-x T0) TANK-Y))
  

;; Game -> Game
;; Produce next game state on every tick
;(define (update-game game) game) ; stub
#;(check-expect (update-game G0)
              (make-game (update-invaders (game-invaders G0))
                         (update-missiles (game-missiles G0))
                         (update-tank (game-tank G0))))

#;
(define (update-game game)
  (make-game (update-invaders (game-invaders game))
             (update-missiles (game-missiles game))
             (update-tank (game-tank game))))

(define (update-game game)
  (game-update-invaders
   (game-update-missiles
    (game-update-tank
     (game-remove-hits game)))))

;; Game -> Game
;; Update tank
(define (game-update-tank game)
  (make-game
   (game-invaders game)
   (game-missiles game)
   (update-tank (game-tank game))))
    
         

;; Game -> Game
;; Remove missiles and invaders that hit each other
(define (game-remove-hits game)
  (make-game
   (remove-hit-invaders (game-invaders game) (game-missiles game))
   (remove-hit-missiles (game-missiles game) (game-invaders game))
   ;(game-missiles game)
   (game-tank game)))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; Produce list of invaders without hits
(define (remove-hit-invaders loi lom)
  (cond
    ((empty? loi) '())
    (else
     (if (invader-is-hit? (first loi) lom)
         (remove-hit-invaders (rest loi) lom)
         (cons (first loi) (remove-hit-invaders (rest loi) lom))))))

;; ListOfMissile ListOfInvader -> ListOfMissile
;; Produce list of missiles without hits
(define (remove-hit-missiles lom loi)
  (cond
    ((empty? lom) '())
    (else
     (if (missile-is-hit? (first lom) loi)
         (remove-hit-missiles (rest lom) loi)
         (cons (first lom) (remove-hit-missiles (rest lom) loi))))))

;; Missile ListOfInvader -> Boolean
;; Detect if missile hit by any of the invaders
(define (missile-is-hit? msl loi)
  (cond
    ((empty? loi) #false)
    (else
     (if (invader-missile-hit? (first loi) msl)
         #true
         (missile-is-hit? msl (rest loi))))))

;; Invader ListOfMissile -> Boolean
;; Detect if Invader is hit by any of the missiles
(define (invader-is-hit? inv lom)
  (cond
    ((empty? lom) #false)
    (else
     (if (invader-missile-hit? inv (first lom))
         #true
         (invader-is-hit? inv (rest lom))))))

;; Invader Missile -> Boolean
;; Return #true is invader and missile are in the hit range
(define (invader-missile-hit? inv msl)
  (and (<= (abs (- (invader-x inv) (missile-x msl))) HIT-RANGE)
       (<= (abs (- (invader-y inv) (missile-y msl))) HIT-RANGE)))      
              
     
;; Game -> Game
;; Update invaders
(define (game-update-invaders game)
  (make-game
   (update-invaders (game-invaders game))
   (game-missiles game)
   (game-tank game)))


;; ListOfInvader -> ListOfInvader
;; Update state of invaders
;(define (update-invaders loi) loi) ; stub

#;(check-expect (update-invaders '())
              (spawn-invader '()))
#;(check-expect (update-invaders LOI1)
              (spawn-invader (move-invaders LOI1)))

(define (update-invaders loi)
  (spawn-invader (move-invaders loi)))

;; ListOfInvader -> ListOfInvader
;; Make new invader at the top with random x and speed

;; Uncomment for production
(define (spawn-invader loi)
  (if (< (random INVADE-RATE) 2)
      (cons (make-invader
             (random (+ 1 WIDTH)) 0
             (* (/ (random 20) 10) (if (zero? (random 2)) -1 1)))
            loi)
      loi))
      
#;(define (spawn-invader loi)
  (cons (make-invader (/ WIDTH 3) 0 2) loi))


;; ListOfInvader -> ListOfInvader
;; Move every invader
;(define (move-invaders loi) loi) ; stub
(check-expect (move-invaders '()) '())
(check-expect (move-invaders LOI1)
              (cons (move-invader I1)
                    (cons (move-invader I4) '())))

(define (move-invaders loi)
  (cond
    ((empty? loi) '())
    (else
     (cons (move-invader (first loi))
           (move-invaders (rest loi))))))

;; Invader -> Invader
;; Move single invader
;(define (move-invader inv) inv) ; stub
(check-expect (move-invader I1)
              (make-invader (+ (invader-x I1) (invader-dx I1))
                            (+ (invader-y I1) (invader-dx I1))
                            (invader-dx I1)))
(define TI1 (make-invader WIDTH 0 10)) ; right edge going right
(check-expect (move-invader TI1)
              (make-invader (+ WIDTH (invader-dx TI1))
                            (+ (invader-y TI1) (abs (invader-dx TI1)))
                            (invader-dx TI1)))
(define TI2 (make-invader (+ WIDTH 10) 0 10)) ; over the right edge going right
(check-expect (move-invader TI2)
              (make-invader WIDTH 10 -10))
(define TI3 (make-invader 0 0 -10)) ; left edge going left
(check-expect (move-invader TI3)
              (make-invader -10 10 -10))
(define TI4 (make-invader -10 0 -10)) ; beyond left edge
(check-expect (move-invader TI4)
              (make-invader 0 10 10))

(define (move-invader inv)
  (cond
    ((> (invader-x inv) INVADER-RIGHT-EDGE)
     (make-invader INVADER-RIGHT-EDGE (invader-y inv) (- (invader-dx inv))))
    ((< (invader-x inv) INVADER-LEFT-EDGE)
     (make-invader INVADER-LEFT-EDGE (invader-y inv) (- (invader-dx inv))))
    (else
     (make-invader (+ (invader-x inv) (invader-dx inv))
                   (+ (invader-y inv) (abs (invader-dx inv)))
                   (invader-dx inv)))))

;; Game -> Game
;; Update missiles
(define (game-update-missiles game)
  (make-game
   (game-invaders game)
   (update-missiles (game-missiles game))
   (game-tank game)))
                            
;; ListOfMissile -> ListOfMissile
;; Update state of missiles
;(define (update-missiles lom) lom) ; stub
(check-expect (update-missiles '()) '())
(check-expect (update-missiles LOM1)
              (remove-missiles (move-missiles LOM1)))

(define (update-missiles lom)
  (remove-missiles (move-missiles lom)))
  


;; ListOfMissile -> ListOfMissile
;; Move all missiles
;(define (move-missiles lom) lom) ; stub
(check-expect (move-missiles LOM1)
              (cons (move-missile M1)
                    (cons (move-missile M4) '())))
(define (move-missiles lom)
  (cond
    ((empty? lom) '())
    (else
     (cons (move-missile (first lom))
           (move-missiles (rest lom))))))

;; Missile -> Missile
;; Move missile
;(define (move-missile m) m) ; stub
(check-expect (move-missile M1)
              (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))
(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissile -> ListOfMissile
;; Remove missiles that are out of the scene
;(define (remove-missiles lom) lom) ; stub
(check-expect (remove-missiles '()) '())
(check-expect (remove-missiles LOM1) LOM1)
(check-expect (remove-missiles (cons (make-missile WIDTH -10) LOM1)) LOM1)

(define (remove-missiles lom)
  (cond
    ((empty? lom) '())
    (else
     (if (< (missile-y (first lom)) 0)
         (remove-missiles (rest lom))
         (cons (first lom) (remove-missiles (rest lom)))))))
               

;; Tank -> Tank
;; Update state of the tank
;(define (update-tank tank) tank) ; stub
(check-expect (update-tank T0) (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0)))
(check-expect (update-tank (make-tank TANK-RIGHT-EDGE 1)) (make-tank TANK-RIGHT-EDGE 1)) ; right edge moving right
(check-expect (update-tank (make-tank (- TANK-RIGHT-EDGE 10) 1)) (make-tank (+ (- TANK-RIGHT-EDGE 10) TANK-SPEED) 1))
(check-expect (update-tank (make-tank (+ TANK-RIGHT-EDGE 3) 1)) (make-tank TANK-RIGHT-EDGE 1)) ; beyond right edge going right
(check-expect (update-tank (make-tank TANK-LEFT-EDGE -1)) (make-tank TANK-LEFT-EDGE -1)) ; moving left
(check-expect (update-tank (make-tank -5 -1)) (make-tank TANK-LEFT-EDGE -1))
(check-expect (update-tank (make-tank (/ WIDTH 4) -1)) (make-tank (- (/ WIDTH 4) TANK-SPEED) -1))

(define (update-tank tank)
  (cond
    ((<=  (+ (tank-x tank) (* (tank-dir tank) TANK-SPEED)) TANK-LEFT-EDGE)
     (make-tank TANK-LEFT-EDGE (tank-dir tank)))
    ((>= (+ (tank-x tank) (* (tank-dir tank) TANK-SPEED)) TANK-RIGHT-EDGE)
     (make-tank TANK-RIGHT-EDGE (tank-dir tank)))
    (else
     (make-tank (+ (tank-x tank) (* (tank-dir tank) TANK-SPEED)) (tank-dir tank)))))











