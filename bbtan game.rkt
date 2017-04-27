#lang racket
(require 2htdp/universe 2htdp/image rsound)

(struct vector (x y) #:transparent)
(struct ball (posn vel) #:mutable)
(struct block (value index) #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GLOBAL VARIABLES

(define height 600)
(define width 500)
(define block-side 50)

(define block-color "deeppink")
(define ball-radius 5)
(define ball-colour "white")
(define plus-color "green")
(define plus-color-outline "darksalmon")
  
(define level 0)
(define score 0)
(define number-of-balls 1)
(define v 500)
(define gap 13)
(define t 0.01)
(define gun-position (vector (/ width 2) height))
(define first-ball #t)
(define increase-blocks #t)


(define llx-background 0)
(define lly-background block-side)
(define rrx-background width)
(define rry-background height)
(define tlc (vector llx-background (+ lly-background block-side)))
(define N (/ width block-side))
(define M (/ height block-side))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GRAPHICAL OBJECTS


(define graphic-ball (circle ball-radius "solid" ball-colour))

(define graphic-block
  (underlay/align "right" "center"
                  (rectangle 5 (- block-side 5) "solid" block-color)
                  (underlay/align "left" "center"
                                  (rectangle 5 (- block-side 5) "solid" block-color)
                                  (underlay/align "center" "top"
                                                  (rectangle (- block-side 5) 5 "solid" block-color)
                                                  (underlay/align "center" "bottom"
                                                                  (rectangle (- block-side 5) 5 "solid" block-color)
                                                                  (square (- block-side 5) "outline" "hotpink"))))))

(define graphic-plus
  (underlay/align "center"
                  "center"
                  (underlay/align "center"
                                  "center"
                                  (rectangle (/ block-side 3) (/ block-side 8) "solid" plus-color)
                                  (rectangle (/ block-side 8) (/ block-side 3) "solid" plus-color))
                  (underlay/align "right" "center"
                                  (rectangle 5 (- block-side 5) "solid" plus-color-outline)
                                  (underlay/align "left" "center"
                                                  (rectangle 5 (- block-side 5) "solid" plus-color-outline)
                                                  (underlay/align "center" "top"
                                                                  (rectangle (- block-side 5) 5 "solid" plus-color-outline)
                                                                  (underlay/align "center" "bottom"
                                                                                  (rectangle (- block-side 5) 5 "solid" plus-color-outline)
                                                                                  (square (- block-side 5) "outline" "lightsalmon")))))))

(define graphic-gun (star-polygon 20 7 3 "solid" "cornflowerblue"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GRAPHIC TEXTS


(define game-over-text (text "GAME-OVER" 80 "red"))

(define score-text (text (string-append "SCORE : " (number->string score)) 50 "orange"))

(define message1 (text "BLAME EVERYTHING AND" 35 "blue"))

(define message2 (text "EVERYONE BUT YOURSELF!!" 35 "blue"))

(define (game-over-screen x) (place-images
                               (list game-over-text (/ width 2) (/ height 4))
                               (list message1 (/ width 2) (/ height 2))
                               (list message2 (/ width 2) (* 0.6 height))
                               (list (text (string-append "SCORE : " (number->string score)) 50 "orange") (/ width 2) (* 0.75 height))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BACKGROUND


(define background (add-line
                    (empty-scene width height "black") llx-background lly-background width lly-background "white"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PLACE IMAGES IS A HIGHER ORDER FUNCTION THAT PLACES ALL THE IMAGES ACCORDING TO THEIR COORDINATES ON THE BACKGROUND


(define (place-images . images-list)
  (foldr (lambda(x y) (place-image (car x) (cadr x) (caddr x) y)) (empty-scene width height "black") images-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CREATES THE RANDOM LIST OF BLOCK  OF NUMBER N 

(define (create-objects n)
  (define (helper x)
    (if (= x 1) (list (* level (random 2)))
        (cons (* level (random 2)) (helper (- x 1)))))
  (define (make-blocks l x)
    (if(null? l) '() 
       (cons (block (car l) (- (+ n 1) x)) (make-blocks (cdr l) (- x 1)))))
    
  (make-blocks (shuffle (append (helper (- n 1)) (list -1))) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MOVE BALLS IS ONE OF THE MAIN PART OF THE PROGRAM WHICH TAKES IN THE COMBINED PART
;AS THE ARGUMENT AND CHECK WHETHER THE BALLS COLLIDE WITH THE BLOCKS OR NOT.
;IT ALSO MOVES THE BALLS ACCORDING TO THEIR VELOCITIES AND ALSO CHANGES THE BLOCK VALUES.


(define (move-balls combined-pair)
  
  (define (move-ball b)
    (let*[(result (collide? b))]
      (cond[(not (equal? result #f))
            (cond
              [(or (equal? result 'top) (equal? result 'bottom))
               (next-posn (ball (ball-posn b) (vector (vector-x (ball-vel b)) (- (vector-y (ball-vel b))))))]
              [(or (equal? result 'left) (equal? result 'right))
               (next-posn (ball (ball-posn b) (vector (- (vector-x (ball-vel b))) (vector-y (ball-vel b)))))])]
           [else (next-posn b)])))
  
  (define (next-posn b)
    (let*[(x (vector-x (ball-posn b)))
          (y (vector-y (ball-posn b)))
          (vx (vector-x (ball-vel b)))
          (vy (vector-y (ball-vel b)))]
      (ball (vector (+ x (* vx t)) (+ y (* vy t))) (vector vx vy))))

  ;THIS IS THE FUNCTION WHICH TAKES IN A SINGLE BALL AS THE ARGUMENT AND CHECKS
  ;WHETHER IT LIES CLOSE TO ANY BLOCK,
  ;IF YES THEN IT GIVES THE SIDE OF THE COLLISION AS THE RESULT.
  ;SIDE CAN BE (TOP , RIGHT , LEFT , BOTTOM).
  
  (define (collide? b)

    (define (helper i result)
      (let*[(v (block-value (list-ref (cdr combined-pair) (- i 1))))]
        (cond
          [(= v 0) #f]
          [(= v -1) (begin (set-block-value! (list-ref (cdr combined-pair) (- i 1)) 0)
                           (set! number-of-balls (+ number-of-balls 1))
                           #f)]
          [else (begin (set-block-value! (list-ref (cdr combined-pair) (- i 1))
                                         (- v 1))
                       (set! score (+ score 1))
                       result)])))
    
    (let* [(x (round (- (vector-x (ball-posn b)) (vector-x tlc))))
           (y (round (- (vector-y (ball-posn b)) (vector-y tlc))))
           (vx (vector-x (ball-vel b)))
           (vy (vector-y (ball-vel b)))
           (row (+ (quotient y block-side) 1))
           (column (+ 1 (quotient x block-side)))
           (k (inexact->exact (+ column (* (- row 1) N))))
           (delta-x (remainder x block-side))
           (delta-y (abs (remainder y block-side)))
           (epsilon 8)]
      (cond
        [(and (<= y (- epsilon block-side)) (<= vy 0)) 'top]
        [(and (>= x (- width epsilon)) (>= vx 0)) 'right]
        [(and (<= x epsilon) (<= vx 0)) 'left]
        [(>= y (- height (vector-y tlc))) #f]
        [(and (< delta-x epsilon) (< delta-y epsilon) (<= vx 0) (<= vy 0) (not (= column 1)) (not (= row 1)))
         (helper (- k (+ N 1)) 'right)]
        [(and (< delta-x epsilon) (< (- block-side delta-y) epsilon) (<= vx 0) (>= vy 0)
              (not (= column 1)) (not (= row M)))
         (helper (- (+ k N) 1) 'right)]
        [(and (< (- block-side delta-x) epsilon) (< (- block-side delta-y) epsilon) (>= vx 0) (>= vy 0)
              (not (= column N)) (not (= row M)))
         (helper (+ k (+ N 1)) 'left)]
        [(and (< (- block-side delta-x) epsilon) (< delta-y epsilon) (>= vx 0) (<= vy 0)
              (not (= column N)) (not (= row 1)))
         (helper (- k (- N 1)) 'left)]
        [(and (< delta-y epsilon) (<= vy 0) (> row 1)) (helper (- k N) 'bottom)]
        [(and (< delta-y epsilon) (>= vy 0) (< y 0)) (helper k 'top)]
        [(and (< (- block-side delta-y) epsilon) (>= vy 0) (> y 0)(not (= row M))) (helper (+ k N) 'top)]
        [(and (< (- block-side delta-y) epsilon) (<= vy 0) (< y 0)) 'top]
        [(and (< (- block-side delta-x) epsilon) (>= vx 0)(>= y 0)) (helper (+ k 1) 'left)]
        [(and (< delta-x epsilon) (<= vx 0)(>= y 0)) (helper (- k 1) 'right)]
        [else #f]))) 
  
  (if (and (null? (car combined-pair)) increase-blocks) (begin
                                                          (set! increase-blocks #f)
                                                          (set! level (+ level 1))
                                                          (cons '() (add-objects N (cdr combined-pair))))

      (let*[(updated-list (map move-ball (car combined-pair)))
            (updated-list-bottom (check-bottom updated-list))]
        
        (cons updated-list-bottom (cdr combined-pair)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS FUNCTION TAKES THE LIST OF BALLS AS ARGUMENTS AND PLACES THEIR GRAPHICAL PART
;ON THE GIVEN BACKGROUND AT ONE INSTANT

(define (place-balls list-of-balls)
  (let* ((graphic-text (text (string-append "LEVEL : " (number->string level) "     "
                                            "BALLS : " (number->string number-of-balls) "     "
                                            "SCORE : " (number->string score)) (/ block-side 2) "orange")))

    (if (null? list-of-balls)
        (place-image graphic-text (/ width 2) (/ block-side 2) background) 

        (let* ((first (car list-of-balls))
               (last (list-ref list-of-balls (- (length list-of-balls) 1))))

          (if (= (length list-of-balls) 1)
              (place-image graphic-ball (vector-x (ball-posn first)) (vector-y (ball-posn first))
                           (place-image graphic-text (/ width 2) (/ block-side 2) background))
              (place-image graphic-ball (vector-x (ball-posn last)) (vector-y (ball-posn last))
                           (place-balls (remove last list-of-balls))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS FUNCTION TAKES A SINGLE BALL AS THE ARGUMENT AND CHECKS WHETHER IT TOUCHES THE BOTTOM
;OF THE PROGRAM OR NOT.

(define (touch-bottom? ball)
  (if (and (> (vector-y (ball-vel ball)) 0) (>= (vector-y (ball-posn ball)) (- rry-background ball-radius))) #t
      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS FUNCTION TAKES THE LIST OF BALLS AS THE ARGUMENTS AND FOR EACH OF THE BALLS CHECKS
;WHETHER THEY TOUCH THE BOTTOM OR NOT
;IF THEY TOUCH THE BOTTOM THEN THEY ARE REMOVED FROM THE LIST.


(define (check-bottom list-of-balls)
  (if (null? list-of-balls) '()
      (if (and (touch-bottom? (car list-of-balls)) first-ball) (begin
                                                                 (set! gun-position (vector (vector-x (ball-posn (car list-of-balls))) height))
                                                                 (set! first-ball #f)
                                                                 (check-bottom (cdr list-of-balls)))
          (if (touch-bottom? (car list-of-balls)) (check-bottom (cdr list-of-balls))
              (cons (car list-of-balls) (check-bottom (cdr list-of-balls)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS FUNCTION TAKES THE PRESENT OBJECT LIST AS THE ARGUMENT AND THE NUMBER OF
;BLOCK TO BE ADDED .
;IT ADDS THE N NUMBER OF NEW OBJECTS IN THE LIST AND REMOVES THE SAME NUMBER OF
;OBJECTS FROM THE TAIL OF THE OBJECT LIST.


(define (add-objects n object-list)
  (let*[(new-objs (create-objects n))
        (last-objs (list-tail object-list (* (- M 1) N)))]
    (append new-objs (map (lambda(o) (block (block-value o) (+ N (block-index o)))) (remove* last-objs object-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS FUNCTION TAKES A SINGLE BLOCK AS THE ARGUMENT AND RETURNS THE POSITION OF THE OF THE
;CENTER OF THE BLOCK.


(define (find-posn block)
  (let* [(i (block-index block))
         (x (if (= 0 (remainder i N)) (* (- N 0.5) block-side)
                (* (- (remainder i N) .5) block-side)))
         (y (if (= 0 (remainder i N)) (abs (* (+ (quotient i N) 0.5) block-side))
                (+ block-side (abs (* (+ (quotient i N) 0.5) block-side)))))]
    (vector x (+ block-side y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS FUNCTION TAKES THE LIST OF OBJECTS AND A BACKGROUND AS THE ARGUMENTS AND PLACES
;THE GRAPHICAL BLOCKS ON THE RESPECTIVE POSITIONS ON THE BACKGROUND ALONGWITH THEIR
;NUMBERS.


(define (place-object list-of-object given-background)
  (if(null? list-of-object) given-background

     (let* ((first (car list-of-object))
            (first-num (number->string (block-value first)))
            (graphic-num (text first-num (/ block-side 2) "yellow"))
            (graphic-block-num (underlay/align "center"
                                               "center"
                                               graphic-block
                                               graphic-num))
            (pos (find-posn first)))
       
       (cond [(= (block-value first) -1) (place-image graphic-plus (vector-x pos) (vector-y pos)
                                                      (place-object (cdr list-of-object) given-background))]
             [(= (block-value first) 0) (place-object (cdr list-of-object) given-background)]
             [else (place-image graphic-block-num (vector-x pos) (vector-y pos)
                                (place-object (cdr list-of-object) given-background))]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS IS THE COMBINED SUPER FUNCTION WHICH INITIATES THE PLACEMENT OF BOTH THE BLOCKS AND THE BALLS
;ALONGWITH THE GRAPHIC GUN.


(define (place-everything combined-pair)
  (place-image graphic-gun (vector-x gun-position) (vector-y gun-position)
               (place-object (cdr combined-pair) (place-balls (car combined-pair)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS FUNCTION IS A PART OF THE BIG BANG.
;IT IS BASICALLY USED TO TAKE THE COORDINATES OF THE CLICKS AND ACCORDINGLY AIM WITH THE GUN


(define (mouse-function combined-pair x y event)
  (if (not (null? (car combined-pair))) combined-pair
      (case event
        [("button-down")
         (begin (set! first-ball #t)
                (set! increase-blocks #t)
                (cons (make-list-of-balls x y) (cdr combined-pair)))]
        [else combined-pair])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS FUNCTION COMES INTO PLAY AFTER A LEVEL INCREASES
;AFTER TAKING COORDINATES OF THE GUN POSITION IT START TO PLACE THE BALLS OUTSIDE THE
;GAMEPLAY SCREEN TO MAKE IT READY TO BE PASSED ON THE REST OF THE FUNCTIONS AND THE BALLS
;MOVE IN THE DIRECTION SPECIFIED BY THE CLICK POSITION.


(define (make-list-of-balls x y)
  (let* ((x-r (- x (vector-x gun-position)))
         (y-r (- y (vector-y gun-position)))
         (r (sqrt (+ (expt x-r 2) (expt y-r 2))))
         (vx (/ (* v x-r) r))
         (vy (/ (* v y-r) r)))
    (define (set-balls i)
      (if (= number-of-balls i) '()
          (cons (ball (vector (- (vector-x gun-position) (* (/ x-r r) i gap))
                              (- (vector-y gun-position) (* (/ y-r r) i gap)))
                      (vector vx vy)) (set-balls (+ i 1)))))
    (set-balls 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS IS THE ACTUAL INITIAL COMBINED LIST WHICH IS EMPTY AND WILL BE PASSED TO THE GAME.

(define initial-state (cons '() (make-list (* (- M 1) N) (block 0 (+ N 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS FUNCTION CHECKS WHETHER ANY BLOCK WHOSE VALUE IS NON-ZERO AND IS NOT A PLUS
;TOUCHES THE BOTTOM (GAME ENDS).

(define (game-over? pair)
  (let* ((objects (list-tail (cdr pair) (* (- M 3) N))))
    (foldr (lambda (x y) (or (> (block-value x) 0) y)) #f objects)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THIS IS THE MAIN FUNCTION WHICH HAS TO BE CALLED TO RUN THE GAME.


(define (play-game)
  (big-bang initial-state
            (on-tick move-balls 1/250)
            (to-draw place-everything)
            (on-mouse mouse-function)
            (stop-when game-over? game-over-screen)))