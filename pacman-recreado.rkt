#lang racket
(require 2htdp/universe)       
(require 2htdp/image)

(define SCREEN-W 800)
(define SCREEN-H 880)
(define TOCK 0.2)

(define E "empty") 
(define D "dot")   
(define W "wall") 
(define C "cherry")


(define INIT-BOARD
  (vector (vector W W W W W W W W W W W W W W W W W W W)
          (vector W D D D D D D D D W D D D D D D D D W)
          (vector W D W W D W W W D W D W W W D W W D W)
          (vector W D D D D D D D D D D D D D D D D D W)
          (vector W D W W D W D D W W W D D W D W W D W)
          (vector W D D D D W W D D W D D W W D D D D W)
          (vector W W W W D W D D D D D D D W D W W W W)
          (vector E E E W D W D W W E W W D W D W E E E)
          (vector W W W W D W D W E E E W D W D W W W W)
          (vector D D D D D D D W W W W W D D D D D D D)
          (vector W W W W D W D D D D D D D W D W W W W)
          (vector E E E W D W D W W W W W D W D W E E E)
          (vector W W W W D D D D D W D D D D D W W W W)
          (vector W D D D D W W W D D D W W W D D D D W)
          (vector W D W W D D D D D W D D D D D W W D W)
          (vector W D D W D W D W W W W W D W D W D D W)
          (vector W W D D D W D D W W W D D W D D D W W)
          (vector W D D W W W W D D W D D W W W W D D W)
          (vector W D D D D D D D D D D D D D D D D D W)
          (vector W W W W W W W W W W W W W W W W W W W)))


(define SMALL-BOARD
  (vector (vector E E E)
          (vector E E E)))

(define CELL-SIZE 40)

(define BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref INIT-BOARD 0))))
(define BOARD-HEIGHT (* CELL-SIZE (vector-length INIT-BOARD)))


(define SMALL-BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref SMALL-BOARD 0))))
(define SMALL-BOARD-HEIGHT (* CELL-SIZE (vector-length SMALL-BOARD)))

(define SCORE-HEIGHT    30)
(define SCORE-TEXT-SIZE 20)

(define PM (overlay/align "right" "middle" 
                          (rotate -45 (right-triangle 14 14 "solid" "black")) 
                          (circle 12 "solid" "yellow")))

(define R-PM PM)
(define U-PM (rotate 90 R-PM))
(define L-PM (rotate 180 R-PM))
(define D-PM (rotate 270 R-PM))
(define C-PM (circle 12 "solid" "yellow")) ;close mouth

(define GT (scale 0.1 (bitmap "cerebro.png")))

(define MTC  (rectangle CELL-SIZE CELL-SIZE "solid" "black")) ; empty cell
(define DTC  (overlay (circle 3  "solid" "white") MTC))       ; dot in cell
(define WALL (rectangle CELL-SIZE CELL-SIZE "solid" "blue"))  ; wall

(define INIT-SCORE  0)

(define (not-wall? cv)
  (not (string=? "wall" cv)))

(define-struct pos (x y))

(define-struct ghost (x y dir))

(define INIT-GT (make-ghost 9 8 "D"))
;; Pos is (make-pos Natural Natural)
;; interp. a position on the board
(define MAP-LIST
  (local [(define i 0)
          (define j 0)
          (define lst empty)
          (define bd INIT-BOARD)]
    (begin 
      (let loopi()
        (when (< i 20)
          (begin
            (set! bd (vector-ref INIT-BOARD i))
            (let loopj()
              (when (< j 19)
                (begin
                  (if (not-wall? (vector-ref bd j))
                      (set! lst (append lst (list (make-pos j i)) ))
                      void)
                  (set! j (add1 j))
                  (loopj))))
            (set! i (add1 i))
            (set! j 0)
            (loopi))))
      lst)))



(define-struct sprite (x y dir))


(define INIT-PM (make-sprite 9 10 "U"))

(define EE-BOARD (vector (vector W W W W)
                         (vector W E E W)
                         (vector W W W W)))

(define-struct gs (pm board board-image score time gt))
;; GameState is (make-gs Sprite Board Image Score Natural Ghost)
;; interp. all parts of the pac-man game; pac-man, the current
;; board, the current board image, and the current score
;; time is the in seconds after the game starts
;; gt is the ghost

(define MTB 
  (empty-scene BOARD-WIDTH
               (+ BOARD-HEIGHT (* 2 SCORE-HEIGHT))
               "black"))

(define SMALL-MTB
  (empty-scene SMALL-BOARD-WIDTH
               (+ SMALL-BOARD-HEIGHT SCORE-HEIGHT)))

(define MTB-GS (make-gs INIT-PM INIT-BOARD MTB INIT-SCORE 0 INIT-GT))


(define R-SPRITE (make-sprite 1 1 "R"))
(define L-SPRITE (make-sprite 1 1 "L"))
(define U-SPRITE (make-sprite 1 1 "U"))
(define D-SPRITE (make-sprite 1 1 "D"))

(define R-GT (make-ghost 2 2 "R"))

;; GameState:
;; MTB-GS previously defined above
(define END-GS (make-gs R-SPRITE EE-BOARD SMALL-MTB 0 0 R-GT))


;; -> GameState
;; runs the game
(define (main)
  (local [(define INIT-GS (make-gs INIT-PM
                                   INIT-BOARD
                                   (render-board INIT-BOARD)
                                   INIT-SCORE
                                   0
                                   INIT-GT))]
    (big-bang INIT-GS
              (on-tick tick TOCK)
              (to-draw render))))


(define (tick gs)
  (local [
          (define board       (gs-board gs))
          (define board-image (gs-board-image gs))
          (define game-time   (gs-time gs))
          (define new-board       (tick-board board))
          (define new-board-image (tick-board-image board board-image))
          ]
    
    (make-gs 
             new-board
             new-board-image
             )))



(define (tick-board bd pm)
  (if (string=? "empty" (board-ref bd (sprite-x pm) (sprite-y pm)))
      bd
      (new-board-w-empty-at (sprite-x pm) (sprite-y pm) bd)))

(define (new-board-w-empty-at x0 y0 bd)
  (map-board (lambda (x y cv)
               (if (and (= x x0) (= y y0))
                   "empty"
                   cv))
             bd))

(define (tick-board-image bd board-image pm)
  (local [(define x (sprite-x pm))
          (define y (sprite-y pm))]
    (if (or (string=? (board-ref bd x y) "dot")
            (string=? (board-ref bd x y) "cherry")) ;cherry is added
        (place-cell-image MTC x y board-image)
        board-image)))


(define (sprite->pos sp)
  (make-pos (sprite-x sp)
            (sprite-y sp)))


(define (count-dots bd)
  (foldr-board (lambda (x y cv b) ;Natural Natural CellValue Natural -> Natural
                 (if (string=? "dot" cv)
                     (add1 b)
                     b))
               0
               bd))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; to-draw handler:
(define (render gs)
  (overlay/align "middle"
                 "center"
                 (above/align "middle"
                              (text "SIMPLE PACMAN" 20 "yellow")
                              (render-time (ceiling (* 0.25 (gs-time gs)))
                                           (render-ghost (gs-gt gs)
                                                         (render-pm (gs-pm gs)
                                                                    (render-score (gs-score gs)
                                                                                  (gs-board-image gs))
                                                                    (gs-time gs))
                                                         (gs-time gs))))
                 (empty-scene SCREEN-W SCREEN-H "black")))

;; Board -> Image
;; draws the board


(define (render-board bd)
  (foldr-board (lambda (x y cv b)
                 (place-cell-image (cell-image cv) x y b))
               MTB
               bd))

(define (render-pm pm img t)
  (local [(define PM
            (cond [(odd? t) C-PM] 
                  [(string=? "U" (sprite-dir pm)) U-PM]
                  [(string=? "D" (sprite-dir pm)) D-PM]
                  [(string=? "L" (sprite-dir pm)) L-PM]
                  [(string=? "R" (sprite-dir pm)) R-PM]))]
    (place-cell-image PM (sprite-x pm) (sprite-y pm) img)))

;; Sprite Image Natural -> Image
;; render the ghost sprite onto img
(define (render-ghost ghost img t)
  (local [(define GT-IMG
            (cond [(= 1 (modulo t 4)) (scale 0.1 (bitmap "cerebro.png"))]
                  [(= 2 (modulo t 4)) (scale 0.1 (bitmap "pinky.png"))]
                  [else
                   GT]))]
    (place-cell-image GT-IMG (ghost-x ghost) (ghost-y ghost) img)))

;; Score Image -> Image
;; adds score to img


(define (render-score score img) 
  (local [(define score-text
            (text (string-append "Score: " (number->string score)) SCORE-TEXT-SIZE "yellow"))]
    (place-image score-text
                 (/ BOARD-WIDTH 2)
                 (+ BOARD-HEIGHT (/ SCORE-HEIGHT 2))
                 img)))
(define (render-time score img) 
  (local [(define score-text
            (text (string-append "Time: " (number->string score)) SCORE-TEXT-SIZE "aqua"))]
    (place-image score-text
                 (/ BOARD-WIDTH 2)
                 (+ 5 BOARD-HEIGHT SCORE-HEIGHT)
                 img)))


;; CellValue -> Image
;; draws a board cell

(define (cell-image cv)
  (cond [(string=? cv "empty") MTC] 
        [(string=? cv "dot")   DTC]
        [(string=? cv "wall")  WALL]))


;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------------
;; Operations on Board and other helpers:


;; Board Natural Natural -> CellValue
;; looks up the value of a Board cell


(define (board-ref bd x y)
  (vector-ref (vector-ref bd y) x))

;(board-ref EE-BOARD 1 1)
;(board-ref ED-BOARD 2 1)
;(board-ref DD-BOARD 3 1)

;; (Natural Natural CellValue -> CellValue) Board -> Board
;; the analogue of map for boards, the function is called for
;; each position in the board to produce a cell value for that
;; position in a new resulting board


(define (map-board fn bd)
  (build-vector (vector-length bd)
                (lambda (y)
                  (build-vector (vector-length (vector-ref bd y))
                                (lambda (x)
                                  (fn x y (board-ref bd x y)))))))

;; (Natural Natural CellValue X -> X) X Board -> X
;; the analogue of foldr for boards, the function is called for
;; each position in the board to produce single value


(define (foldr-board fn base bd)
  (local [(define nrows (vector-length bd))
          (define ncols (vector-length (vector-ref bd 0)))
          
          (define (rows y b)
            (cond [(= y nrows) b]
                  [else
                   (rows (add1 y)
                         (cols 0 y b))]))
          (define (cols x y b)
            (cond [(= x ncols) b]
                  [else
                   (cols (add1 x)
                         y
                         (fn x y (board-ref bd x y) b))]))]
    (rows 0 base)))

;; Image Natural Natural Image -> Image
;; adds cell-img to board-image at x, y board coordinates


(define (place-cell-image cell-img x y board-image)
  (place-image cell-img
               (+ (* x CELL-SIZE) (/ CELL-SIZE 2))
               (+ (* y CELL-SIZE) (/ CELL-SIZE 2))
               board-image))

;; starting the game
(main)
