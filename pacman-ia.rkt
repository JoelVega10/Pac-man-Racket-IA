#lang racket
(require 2htdp/universe)       
(require 2htdp/image)

;; Constantes

(define SCREEN-W 1024)
(define SCREEN-H 600)
(define TOCK 0.1)

(define E "empty") 
(define D "dot")   
(define W "wall") 

;;Tabla Inicial del nivel 1. y tabla pequena.

(define INIT-BOARD
  (vector (vector W W W W W W W W W W W W W W W W W W W W )
          (vector W D D D W W D D D D D D D D W W D D D W )
          (vector W D W D D D D W W W W W W D D D D W D W )
          (vector W D W D W W D D D D D D D D W W D W D W )
          (vector W D W D W W D W W W W W W D W W D W D W )
          (vector W D D D D D D D D D D D D D D D D D D W )
          (vector W W W W W W W W W W W W W W W W W W W W )))

(define SMALL-BOARD
  (vector (vector E E E)
          (vector E E E)))

;;Tamano de celdas.

(define CELL-SIZE 40)

;;Establecimiento de altura y anchuras de tablas.

(define BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref INIT-BOARD 0))))
(define BOARD-HEIGHT (* CELL-SIZE (vector-length INIT-BOARD)))

(define SMALL-BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref SMALL-BOARD 0))))
(define SMALL-BOARD-HEIGHT (* CELL-SIZE (vector-length SMALL-BOARD)))

;;Datos basicos a utilizar mas adelante en la interfaz.

(define SCORE-HEIGHT    30)
(define SCORE-TEXT-SIZE 20)

(define PM (overlay/align "right" "middle" 
                          (rotate -45 (right-triangle 14 14 "solid" "black")) 
                          (circle 12 "solid" "yellow")))

;; Para utilizar mas adelante y lograr un efecto de rotacion de pacman.

(define R-PM PM)
(define U-PM (rotate 90 R-PM))
(define L-PM (rotate 180 R-PM))
(define D-PM (rotate 270 R-PM))
(define C-PM (circle 12 "solid" "yellow"))

;;Estableciendo imagen para el fantasma

(define GT (scale 0.1 (bitmap "cerebro.png")))

;;Imagenes para las celdas.

(define MTC  (rectangle CELL-SIZE CELL-SIZE "solid" "black")) ; empty cell
(define DTC  (overlay (circle 3  "solid" "white") MTC))       ; dot in cell
(define WALL (rectangle CELL-SIZE CELL-SIZE "solid" "blue"))  ; wall


(define MTB 
  (empty-scene BOARD-WIDTH
               (+ BOARD-HEIGHT (* 2 SCORE-HEIGHT))
               "black"))

(define SMALL-MTB
  (empty-scene SMALL-BOARD-WIDTH
               (+ SMALL-BOARD-HEIGHT SCORE-HEIGHT)))

;;Inicializar el score en cero.

(define INIT-SCORE  0)

#|
Nombre : not-wall?
Parametros : cv -> celda en la tabla.
Descripcion: verifica si en la celda enviada hay o no un muro.
|#

(define (not-wall? cv)
  (not (string=? "wall" cv)))

#|
Nombre : isPoint
Parametros : cv -> celda en la tabla.
Descripcion: verifica si en la celda enviada es un punto.
|#
(define (isPoint cv)
  (string=? "dot" cv)
  )

;;Struct que nos permite guardar posiciones exactas conformadas por coordenadas x y.
(define-struct pos (x y))

;;Struct que nos permite guardar posiciones exactas conformadas por coordenadas x y ademas de un simbolo que representa una imagen, se utiliza para la creacion de pacman.
(define-struct sprite (x y dir))

;;Struct que nos permite guardar posiciones exactas conformadas por coordenadas x y ademas de un simbolo que representa una imagen, se utiliza para la creacion de un fantasma.
(define-struct ghost (x y dir))

;;Se define la posicion inicial de pacman.
(define INIT-PM (make-sprite 9 5 "U"))

;;Se define la posicion inicial del fantasma.
(define INIT-GT (make-ghost 18 1 "D"))

;;Se define un fantasma que se utiliza para simplemente almacenar la posicion anterior del fantasma.
(define oldGhost (make-ghost 1 1 "D"))

;;Struct que representa un game state, un game state va contener un pacman, un tablero, la imagen que representa el mismo, una puntuacion, un tiempo y un fantasma.
(define-struct gs (pm board board-image score time gt))


;; Sprites para variar las caras de pacman.
(define R-SPRITE (make-sprite 1 1 "R"))
(define L-SPRITE (make-sprite 1 1 "L"))
(define U-SPRITE (make-sprite 1 1 "U"))
(define D-SPRITE (make-sprite 1 1 "D"))

(define R-GT (make-ghost 2 2 "R"))

;;Representa una tabla vacia.
(define EE-BOARD (vector (vector W W W W)
                         (vector W E E W)
                         (vector W W W W)))


;;Representa el game state para finalizar el juego.
(define END-GS (make-gs R-SPRITE EE-BOARD SMALL-MTB 0 0 R-GT))


#|
Nombre : main
Parametros : ninguno.
Descripcion: Inicia el juego mediante el uso de big-bang.
|#
(define (main)
  (local [(define INIT-GS (make-gs INIT-PM INIT-BOARD (render-board INIT-BOARD) INIT-SCORE 0 INIT-GT ))]
    (big-bang INIT-GS
              (on-tick tick TOCK)
              (to-draw render)
              (stop-when game-over? last-scene))))

#|
Nombre : final-score
Parametros : ttime -> tiempo final, tscore -> puntuacion final.
Descripcion: Calcula la puntuacion final.
|#
(define (final-score ttime tscore)
  (- tscore  ttime))

#|
Nombre : last-scene
Parametros : gs -> estado del juego al terminar.
Descripcion: Muestra la escena final.
|#
(define (last-scene gs)
  (cond [(goal? (ghost->pos (gs-gt gs)) (sprite->pos (gs-pm gs)))
         (overlay/align "middle" "middle" 
                        (above (text "GAME OVER." 20 "white")
                               (text "Tiempo Total:" 20 "white")
                               (text (number->string (ceiling (* TOCK (gs-time gs)))) 20 "white")
                               (text "Score Total:" 20 "white")
                               (text (number->string (gs-score gs)) 20 "white")
                               )
                        (empty-scene SCREEN-W SCREEN-H "black"))]        
        [else (overlay/align "middle" "middle" 
                             (above (text "YOU WIN ! ." 20 "white")
                                    (text "Tiempo Total:" 20 "white")
                                    (text (number->string (ceiling (* TOCK (gs-time gs)))) 20 "white")
                                    (text "Score Total:" 20 "white")
                                    (text (number->string (gs-score gs)) 20 "white")
            
                                    )
                             (empty-scene SCREEN-W SCREEN-H "black"))]))

#|
Nombre : getPosMovesGhost
Parametros : GhostPos -> posicion del fantasma enviada.
Descripcion: Se encarga de verificar y retornar una lista de movimientos validos para el fantasma en la posicion recibida.
|#
(define (getPosMovesGhost GhostPos)
  (local[(define moves empty)]
   (begin
      (if (and (not-wall?(board-ref INIT-BOARD (pos-x GhostPos) ( + (pos-y GhostPos) 1) )) (not(equal? ( + (pos-y GhostPos) 1) (ghost-y oldGhost) ) ) ) ;;Verifica si no hay muro y no es igual a la posicion anterior(para evitar ciclos).
          (set! moves (append moves (list (make-pos (pos-x GhostPos) ( + (pos-y GhostPos) 1) )) ))
          void)
      (if (and (not-wall?(board-ref INIT-BOARD (pos-x GhostPos) ( - (pos-y GhostPos) 1) )) (not(equal? ( - (pos-y GhostPos) 1) (ghost-y oldGhost))))
          (set! moves (append moves (list (make-pos (pos-x GhostPos) ( - (pos-y GhostPos) 1) )) ))
          void)
      (if (and (not-wall?(board-ref INIT-BOARD (+ (pos-x GhostPos) 1) (pos-y GhostPos) )) (not(equal? (+ (pos-x GhostPos) 1) (ghost-x oldGhost))))
          (set! moves (append moves (list (make-pos (+ (pos-x GhostPos) 1) (pos-y GhostPos) )) ))
          void)
      (if (and (not-wall?(board-ref INIT-BOARD (- (pos-x GhostPos) 1) (pos-y GhostPos) )) (not(equal? (- (pos-x GhostPos) 1) (ghost-x oldGhost))))
          (set! moves (append moves (list (make-pos (- (pos-x GhostPos) 1) (pos-y GhostPos) )) ))
          void)
    )
   moves)
 )

#|
Nombre : getPosMoves
Parametros : PacmanPos -> posicion de pacman enviada.
Descripcion: Se encarga de verificar y retornar una lista de movimientos validos para pacman en la posicion recibida.
|#
(define (getPosMoves PacmanPos)
  (local[(define moves empty)]
   (begin
      (if (not-wall?(board-ref INIT-BOARD (pos-x PacmanPos) ( + (pos-y PacmanPos) 1) ))  ;Verifica que en la nueva posicion no hay un muro.
          (set! moves (append moves (list (make-pos (pos-x PacmanPos) ( + (pos-y PacmanPos) 1) )) ))
          void)
      (if (not-wall?(board-ref INIT-BOARD (pos-x PacmanPos) ( - (pos-y PacmanPos) 1) ))
          (set! moves (append moves (list (make-pos (pos-x PacmanPos) ( - (pos-y PacmanPos) 1) )) ))
          void)
      (if (not-wall?(board-ref INIT-BOARD (+ (pos-x PacmanPos) 1) (pos-y PacmanPos) ))
          (set! moves (append moves (list (make-pos (+ (pos-x PacmanPos) 1) (pos-y PacmanPos) )) ))
          void)
      (if (not-wall?(board-ref INIT-BOARD (- (pos-x PacmanPos) 1) (pos-y PacmanPos) ))
          (set! moves (append moves (list (make-pos (- (pos-x PacmanPos) 1) (pos-y PacmanPos) )) ))
          void)
    )
   moves)
 )

#|
Nombre : ghost-stuff
Parametros : possibleMove -> Movimiento al cual se quiere obtener score, ghosts -> estructura de fantasma (posicion),
radius -> cantidad de celdas hacia adelante que se busca un fantasma, scores -> score obtenido por la funcion de eval
Descripcion: Verifica si en el camino a donde se quiere mover hay un fantasma y le resta al score.
|#
(define (ghost-stuff possibleMove ghosts radius scores)
  (cond
    ((empty? ghosts) scores)
    ((<= (h-score (car ghosts) possibleMove) radius)
     ghost-stuff possibleMove (cdr ghosts) radius (- scores 300))
    (else (ghost-stuff possibleMove (cdr ghosts) radius scores))))

#|
Nombre : closest-dot
Parametros : curPos -> Posicion a donde se quiere mover, foodPos -> Lista de posiciones en donde hay puntos
Descripcion: Verifica cual es punto mas cercano despues de realizar el movimiento y devuelve la distancia que hay de pacman a este.
|#
(define (closest-dot curPos foodPos)
  (local [
          (define foodDistances empty)
          ]
    (begin
           (for ([food foodPos])
              (set! foodDistances (append foodDistances (list(h-score food curPos)))))
             )
          (closest-dot-return foodDistances)))

#|
Nombre : minimum-number
Parametros : lst -> una lista de numeros , acc -> primera posicion de la lista
Descripcion: Saca el minimo numero de la lista y lo retorna.
|#
(define (minimum-number lst acc)
  (cond
    ((null? lst) acc)
    ((< (car lst)  acc)
     (minimum-number (cdr lst) (car lst)))
    (else
     (minimum-number (cdr lst) acc))))

#|
Nombre : mini-list
Parametros : lst -> una lista de numeros
Descripcion: Llama a minimum-number para sacar el minimo de una lista.
|#
(define (mini lst)
  (if (null? lst)
      #f
      (minimum-number (cdr lst) (car lst))))

#|
Nombre : closest-dot-return
Parametros : lst -> una lista de numeros
Descripcion: Si esta vacia retorna 1 sino retorna el minimo de la lista para obtener la menor distancia en closest-dot
|#
(define (closest-dot-return lst)
  (cond
    ((empty? lst) 1)
    (else(mini lst))))

#|
Nombre : sum-food-proximity 
Parametros : curPos -> posicion a la que se quiere mover, foodPos -> lista de posiciones en donde se encuentran los puntos en el tablero.
Descripcion: Retorna la cantidad de puntos que existen en el laberinto en el momento solicitado.
|#
(define (sum-food-proximity curPos foodPos)
  (local [
          (define foodDistances empty)
          ]
    (begin
           (for ([food foodPos])
              (set! foodDistances (append foodDistances (list (h-score food curPos)))))
             )
          (sum-food-proximity-return foodDistances)))


#|
Nombre : my-sum 
Parametros : L -> lista de numeros
Descripcion: Retorna la cantidad la suma de los elementos de la lista.
|#
(define (mySum L)
  (apply + L))

#|
Nombre : sum-food-proximity-return 
Parametros : lst -> lista de numeros
Descripcion: Retorna la cantidad la suma de los elementos de la lista y si la lista esta vacia retorna cero para conocer la cantidad de puntos que hay en el mapa.
|#
(define (sum-food-proximity-return lst)
  (cond
    ((> (mySum lst) 0) (mySum lst))
    (else 1)))

#|
Nombre : food-stuff
Parametros : curPos -> Posicion a donde se quiere mover, foodPos -> Lista de posiciones en donde hay puntos, curScore -> Score actual de la funcion eval
pastMove -> posicion en donde se encuentra pac-man, pastPoints -> cantidad de puntos antes de realizar el movimiento 
Descripcion: Retorna el score de acuerdo a si se encuentra un punto o un espacio vacio.
|#
(define (food-stuff curPos foodPos curScore pastMove pastPoints)
 (cond
    ((> (/ 1 (sum-food-proximity curPos foodPos)) (/ 1 (sum-food-proximity pastMove pastPoints)))(food-stuff2 curPos foodPos (+ curScore (* 100 (- (/ 1 (sum-food-proximity curPos foodPos)) (/ 1 (sum-food-proximity pastMove pastPoints))))) pastMove pastPoints))
    (else(food-stuff2 curPos foodPos (- curScore 20) pastMove pastPoints)))
  )

#|
Nombre : food-stuff2
Parametros : curPos -> Posicion a donde se quiere mover, foodPos -> Lista de posiciones en donde hay puntos, curScore -> Score actual de la funcion eval
pastMove -> posicion en donde se encuentra pac-man, pastPoints -> cantidad de puntos antes de realizar el movimiento 
Descripcion: Retorna el score de acuerdo a si se encuentra un punto o un espacio vacio.
|#
(define (food-stuff2 curPos foodPos curScore pastMove pastPoints)
 (cond
    ((> (closest-dot curPos foodPos) (closest-dot pastMove pastPoints)) (+ curScore (* 100 (- (closest-dot curPos foodPos) (closest-dot pastMove pastPoints) ))))
    (else(+ curScore 20)))
  )


#|
Nombre : evaluationFunction
Parametros : state -> un estado de juego por evaluar, pastMove -> la posicion anterior a la del estado enviado, pastPoints -> cantidad de puntos en el estado anterior.
Descripcion: Se encarga de inicializar y aplicar todas las evaluaciones detalladas mas arriba.
|#
(define (evaluationFunction state pastMove pastPoints)
  (food-stuff (sprite->pos(gs-pm state)) (DPos (gs-board state)) (ghost-stuff (sprite->pos(gs-pm state)) (list (ghost->pos (gs-gt state))) 4 (gs-score state) ) pastMove pastPoints )
 )

#|
Nombre : evalState
Parametros : state -> un estado de juego por evaluar.
Descripcion: No es utilizada pero esta es otra funcion que realiza otra forma de evaluacion mas simple.
|#
(define (evalState state)
    (local[(define score 0)
           (define totalPoints (DPos INIT-BOARD))
           (define dotsEaten (* (- (length (DPos (gs-board state))) (length totalPoints) ) -1))
           ]
      (begin
        (+ score (* 2 dotsEaten))
        (+ score (closest-dot (sprite->pos(gs-pm state)) (DPos (gs-board state)) ))
        (+ score (ghost-stuff (sprite->pos(gs-pm state)) (list (ghost->pos (gs-gt state))) 5 score ))
        )
      score)
 )

#|
Nombre : distancia
Parametros : pos->posicion xy, d-> distancia entre pacman y otro objeto
Descripcion: Retorna un struct con una posicion y distancia
|#
(define-struct distancia (pos d) #:transparent)

#|
Nombre : h-score 
Parametros : p1-> posicion p2 -> posicion
Descripcion: Retorna la distancia de manhattan entre dos puntos.
|#
(define (h-score p1 p2)
            (+ (abs (- (pos-x p1) (pos-x p2)))
               (abs (- (pos-y p1) (pos-y p2)))))

#|
Nombre : minimum
Parametros : lst -> una lista de distancias , acc -> primera posicion de la lista
Descripcion: Saca el minimo numero de la lista y lo retorna.
|#
(define (minimum lst acc)
  (cond
    ((null? lst) acc)
    ((< (distancia-d (car lst)) (distancia-d acc))
     (minimum (cdr lst) (car lst)))
    (else
     (minimum (cdr lst) acc))))

#|
Nombre : mymin
Parametros : lst -> una lista de distancias
Descripcion: Llama a minimum para sacar el minimo de una lista.
|#
(define (mymin lst)
  (if (null? lst)
      #f
      (minimum (cdr lst) (car lst))))

#|
Nombre : getScoreGhost 
Parametros : GHCurPos -> posicion actual del fantasma, PMCurPos -> posicion actual del pacman
Descripcion: Obtiene la direccion en donde hay menor distancia para llegar a pacman.
|#
(define (getScoreGhost GHCurPos PMCurPos)
    (local [(define possibleMoves (getPosMovesGhost GHCurPos))
            
            (define distances empty)
            ]
        (begin
           (set! oldGhost (make-ghost (pos-x GHCurPos) (pos-y GHCurPos) "D"))
           (for ([move possibleMoves])
              (set! distances (append distances (list (make-distancia move (h-score move PMCurPos)))))
             )
          )
      (list(distancia-pos (mymin distances)))))


#|
Nombre : checkCaseforDot
Parametros : board -> una tabla de un estado especifico, pm -> posicion de pacman en esa tabla, score -> puntuacion actual en ese estado.
Descripcion: Revisa si en la casilla donde se movio pacman habia un punto y lo suma a la puntuacion de ese estado y lo retorna.
|#
(define (checkCaseforDot board pm score)
   (if (string=? "dot" (board-ref board (sprite-x pm) (sprite-y pm)))
      (+ score 5)
      score)
 )

#|
Nombre : checkCase
Parametros : board -> una tabla de un estado especifico, pm -> posicion de pacman en esa tabla.
Descripcion: Revisa si en la casilla donde se movio pacman habia un punto y si lo habia lo limpia en la tabla y lo retorna.
|#
(define (checkCase board pm)
   (if (string=? "empty" (board-ref board (sprite-x pm) (sprite-y pm)))
      board
      (new-board-w-empty-at (sprite-x pm) (sprite-y pm) board))
 )

#|
Nombre : make-new-state-pm
Parametros : state -> un estado del juego segun el struct gs, move -> el movimiento por aplicar a pacman en el estado.
Descripcion: Crea y retorna un estado donde se efectua el movimiento recibido a pacman.
|#
(define (make-new-state-pm state move)
  (local[(define pm (make-sprite (pos-x move) (pos-y move) (sprite-dir (gs-pm state))))
         (define newBD (checkCase (gs-board state) pm))
         (define newScore (checkCaseforDot (gs-board state) pm (gs-score state)))
         (define newState (make-gs
             pm
             newBD
            (gs-board-image state)
             newScore
            (gs-time state)
            (gs-gt state)
             ))]
    newState)
 )

#|
Nombre : make-new-state-ghost
Parametros : state -> un estado del juego segun el struct gs, move -> el movimiento por aplicar a un fantasma en el estado.
Descripcion: Crea y retorna un estado donde se efectua el movimiento recibido a el fantasma.
|#
(define (make-new-state-ghost state move)
  (local[(define ghost (make-ghost (pos-x move) (pos-y move) "D" ))
         (define newState (make-gs
            (gs-pm state)
            (gs-board state)
            (gs-board-image state)
            (gs-score state)
            (gs-time state)
             ghost
             ))]
    newState)
 )

#|
Nombre : MaxTurn
Parametros : state -> un estado del juego segun el struct gs, depth -> la profundidad actual del algoritmo minimax, alpha -> el valor alpha para la aplicacion de la poda alpha-beta,
 beta -> el valor alpha para la aplicacion de la poda alpha-beta, h -> valor de profundidad maximo.
Descripcion: En esta funcion junto con la funcion a continuacion de MinTurn se aplica el algoritmo minimax para definir el movimiento de pacman.
|#
(define (MaxTurn state depth alpha beta h)
  (local[(define pm (gs-pm state))
         (define possibleMoves (getPosMoves (sprite->pos pm)))
         (define best-score -100000)
         (define score best-score)
         (define best-move (sprite->pos pm))
         ]
    (begin
      (for ([move possibleMoves])
        (begin
            (set! state (make-new-state-pm state move))
            (set! score (MinTurn state depth alpha beta h))
            (if (> score best-score)
                (begin
                  (set! best-score score)
                  (set! best-move move)
                 )
                void)
            (set! alpha (max alpha best-score))
            (if (> best-score beta) best-score void)
          )
        )
       (if (equal? depth 0) best-move best-score)
      )
    )
)

#|
Nombre : MinTurn
Parametros : state -> un estado del juego segun el struct gs, depth -> la profundidad actual del algoritmo minimax, alpha -> el valor alpha para la aplicacion de la poda alpha-beta,
 beta -> el valor alpha para la aplicacion de la poda alpha-beta, h -> valor de profundidad maximo.
Descripcion: En esta funcion junto con la funcion anterior de MaxTurn se aplica el algoritmo minimax para definir el movimiento de pacman.
|#
(define (MinTurn state depth alpha beta h)
  (local[(define ghost (gs-gt state))
         (define possibleMoves (getPosMovesGhost (ghost->pos ghost) ))
         (define best-score 100000)
         (define score best-score)
         (define pastMove (sprite->pos(gs-pm state)))
         (define pastPoints (DPos (gs-board state)))
         ]
    (begin
       (for ([move possibleMoves])
        (begin
            (set! state (make-new-state-ghost state move))
            (if (equal? depth h)
                (set! score (evaluationFunction state pastMove pastPoints) )
                (set! score (MaxTurn state (add1 depth) alpha beta h))
            )
            (if (< score best-score) (set! best-score score) void)
            (set! beta (min beta best-score))
            (if (< best-score alpha) best-score void)
          )
        )   
      best-score)
    )
 )

#|
Nombre : DPos
Parametros : board -> un tabla cualquiera.
Descripcion: Funcion simple que retorna una lista de posiciones donde hay puntos.
|#
(define (DPos board)
  (local [(define i 0)
          (define j 0)
          (define lst empty)
          (define bd board)]
    (begin 
      (let loopi()
        (when (< i 7)
          (begin
            (set! bd (vector-ref INIT-BOARD i))
            (let loopj()
              (when (< j 20)
                (begin
                  (if (isPoint (vector-ref bd j))
                      (set! lst (append lst (list (make-pos j i)) ))
                      void)
                  (set! j (add1 j))
                  (loopj))))
            (set! i (add1 i))
            (set! j 0)
            (loopi))))
      lst)))



#|
Nombre : tick 
Parametros : gs -> recibe un estado de juego especifico.
Descripcion: Es la encargada de refresacar el juego luego de cada tick.
|#
(define (tick gs)
  (local [(define pm          (gs-pm gs))
          (define board       (gs-board gs))
          (define board-image (gs-board-image gs))
          (define score       (gs-score gs))
          (define game-time   (gs-time gs))
          (define ghost        (gs-gt gs))
          (define new-pm          (tick-pm pm board game-time gs))
          (define new-board       (tick-board board new-pm))
          (define new-board-image (tick-board-image board board-image new-pm))
          (define new-score       (tick-score new-pm board score))
          (define new-time        (tick-time score new-pm board game-time))
          (define new-ghost       (tick-ghost ghost new-pm new-board game-time))
          ]
    
    (make-gs new-pm
             new-board
             new-board-image
             new-score
             new-time
             new-ghost
             )))


#|
Nombre : tick-time 
Parametros : score -> puntuacion que se utiliza para verificar si se ha iniciado el juego, new-pm -> posicion de pacman, last-board -> tabla anterior, t -> tiempo a mostrar.
Descripcion: Es la encargada de modificar el tiempo para la siguiente imagen a mostrar.
|#
(define (tick-time score new-pm last-board t)
  (local [(define pos (board-ref last-board (sprite-x new-pm) (sprite-y new-pm)))]
    (cond [(=  score 0.0) 0.0]
          [else
           (ceiling (add1 t))])))

#|
Nombre : tick-ghost 
Parametros : ghost -> posicion del fantasma, pm -> posicion de pacman, bd -> tabla actual, t -> tiempo transcurrido.
Descripcion: Es la encargada de modificar la posicion del fantasma aplicando el movimiento segun un algoritmo simple utilizando distancia manhattan para luego mostrar este resultado.
|#
(define (tick-ghost ghost pm bd t)
  (if (zero? (modulo t 6))
      ghost
      (if (integer? t)
          (if (goal? (ghost->pos ghost) (sprite->pos pm))
              ghost
              (local [(define p 
                        (local [(define search-result (getScoreGhost (ghost->pos ghost) (sprite->pos pm)))]
                          (if (false? search-result) (ghost->pos ghost) (first search-result))))
                      (define x (pos-x p))
                      (define y (pos-y p))]
                (begin
                (make-ghost x y (ghost-dir ghost))
                )
                )
              )
          ghost)))




#|
Nombre : tick-pm 
Parametros : pm -> posicion de pacman, bd -> tabla actual, t -> tiempo transcurrido, gs -> recibe un estado de juego especifico.
Descripcion: Es la encargada de modificar la posicion de pacman aplicando el movimiento segun el algoritmo minimax con poda alpha-beta para luego mostrar este resultado.
|#
(define (tick-pm pm bd t gs)
  (if (zero? (modulo t 6))
      pm
      (if (integer? t)
          (if (empty? (DPos bd))
              pm
              (local [(define p 
                        (local [(define search-result (list(MaxTurn gs 0 -100000 100000 2)) )]
                          (if (false? search-result) (sprite->pos pm) (first search-result))))
                      (define x (pos-x p))
                      (define y (pos-y p))]
                (make-sprite x y (sprite-dir pm))))
          pm))
  )

#|
Nombre : tick-board 
Parametros : bd -> tabla actual, pm -> posicion de pacman.
Descripcion: Modifica la celfa de la tabla en caso de pacman pasa por un punto y mostrar esto.
|#
(define (tick-board bd pm)
  (if (string=? "empty" (board-ref bd (sprite-x pm) (sprite-y pm)))
      bd
      (new-board-w-empty-at (sprite-x pm) (sprite-y pm) bd)))

#|
Nombre : new-board-w-empty-at 
Parametros : x0 -> posicion x a revisar, y0 -> posicion y a revisar, bd -> tabla actual.
Descripcion: Limpia la celda en la poscion enviada y retorna la nueva tabla.
|#
(define (new-board-w-empty-at x0 y0 bd)
  (map-board (lambda (x y cv) ; Natural Natural CellValue -> CellValue
               (if (and (= x x0) (= y y0)) "empty" cv))
             bd))

#|
Nombre : tick-board-image 
Parametros : bd -> tabla actual, board-image -> ta tabla imagen actual, pm -> posicion de pacman.
Descripcion: Modifica las celdas de la tabla imagen.
|#
(define (tick-board-image bd board-image pm)
  (local [(define x (sprite-x pm))
          (define y (sprite-y pm))]
    (if (string=? (board-ref bd x y) "dot")
        (place-cell-image MTC x y board-image)
        board-image)))

#|
Nombre : tick-score 
Parametros : new-pm -> posicion nueva de pacman, last-board -> tabla anterior al movimiento, score -> puntuacion actual.
Descripcion: Modifica las celdas de la tabla imagen.
|#
(define (tick-score new-pm last-board score)
  (local [(define pos (board-ref last-board (sprite-x new-pm) (sprite-y new-pm)))]
    (cond [(string=? "dot" pos)
           (+ 5 score)]
          [else
           score])))

#|
Nombre : sprite->pos 
Parametros : sp -> sprite por buscarle la posicion.
Descripcion: retorna la posicion de un struct pos.
|#
(define (sprite->pos sp)
  (make-pos (sprite-x sp)
            (sprite-y sp)))

#|
Nombre : ghost->pos 
Parametros : gt -> ghost por buscarle la posicion.
Descripcion: retorna la posicion de un struct ghost.
|#
(define (ghost->pos gt)
  (make-pos (ghost-x gt)
            (ghost-y gt)))

#|
Nombre : goal?
Parametros : Pos Pos -> Boolean
Descripcion: verifica si una posicion es igual a la otra.
|#
(define (goal? p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))))

#|
Nombre : game-over?
Parametros : GameState -> Boolean
Descripcion: verifica si pacman comio todos los puntos del tablero.
|#
(define (game-over? gs)
  (or (zero? (count-dots (gs-board gs)))
      (goal? (ghost->pos (gs-gt gs)) (sprite->pos (gs-pm gs)))))


#|
Nombre : count-dots
Parametros : tablero -> Natural
Descripcion: cuenta los puntos en el tablero
|#
(define (count-dots bd)
  (foldr-board (lambda (x y cv b) ;Natural Natural CellValue Natural -> Natural
                 (if (string=? "dot" cv)
                     (add1 b)
                     b))
               0
               bd))



#|
Nombre : render-gs
Parametros : GameState -> Image
Descripcion: pinta el conjunto de juego en pantalla
|#
(define (render gs)
  (overlay/align "middle"
                 "center"
                 (above/align "middle"
                              (text "PacMan IA" 20 "white")
                              (render-time (ceiling (* 0.25 (gs-time gs)))
                                           (render-ghost (gs-gt gs)
                                                         (render-pm (gs-pm gs)
                                                                    (render-score (gs-score gs)
                                                                                  (gs-board-image gs))
                                                                    (gs-time gs))
                                                         (gs-time gs))))
                 (empty-scene SCREEN-W SCREEN-H "black")))

#|
Nombre : render-pm
Parametros : tablero -> Image
Descripcion: pinta el tablero en pantalla
|#
(define (render-board bd)
  (foldr-board (lambda (x y cv b)
                 (place-cell-image (cell-image cv) x y b))
               MTB
               bd))

#|
Nombre : render-pm
Parametros :Sprite Image Natural -> Image
Descripcion: pinta el pacman en pantalla
|#
(define (render-pm pm img t)
  (local [(define PM
            (cond [(odd? t) C-PM] 
                  [(string=? "U" (sprite-dir pm)) U-PM]
                  [(string=? "D" (sprite-dir pm)) D-PM]
                  [(string=? "L" (sprite-dir pm)) L-PM]
                  [(string=? "R" (sprite-dir pm)) R-PM]))]
    (place-cell-image PM (sprite-x pm) (sprite-y pm) img)))

#|
Nombre : render-ghost
Parametros : Sprite Image Natural -> Image
Descripcion: pinta el fantasma en pantalla
|#
(define (render-ghost ghost img t)
  (local [(define GT-IMG
            (cond [(= 1 (modulo t 4)) (scale 0.1 (bitmap "cerebro.png"))]
                  [(= 2 (modulo t 4)) (scale 0.1 (bitmap "pinky.png"))]
                  [else
                   GT]))]
    (place-cell-image GT-IMG (ghost-x ghost) (ghost-y ghost) img)))

#|
Nombre : render-score
Parametros : Score Image -> Image
Descripcion: pinta el score en pantalla
|#
(define (render-score score img) 
  (local [(define score-text
            (text (string-append "Puntuacion: " (number->string score)) SCORE-TEXT-SIZE "white"))]
    (place-image score-text
                 (/ BOARD-WIDTH 2)
                 (+ BOARD-HEIGHT (/ SCORE-HEIGHT 2))
                 img)))

#|
Nombre : render-time
Parametros : time Image -> Image
Descripcion: pinta el tiempo en pantalla
|#
(define (render-time score img) 
  (local [(define score-text
            (text (string-append "Tiempo: " (number->string score)) SCORE-TEXT-SIZE "white"))]
    (place-image score-text
                 (/ BOARD-WIDTH 2)
                 (+ 5 BOARD-HEIGHT SCORE-HEIGHT)
                 img)))

#|
Nombre : cell-image
Parametros : ValorCelda -> Image
Descripcion: pinta una celda del tablero
|#
(define (cell-image cv)
  (cond [(string=? cv "empty") MTC] 
        [(string=? cv "dot")   DTC]
        [(string=? cv "wall")  WALL]
        ))

#|
Nombre : board-ref
Parametros : tablero Natural Natural -> ValorCelda
Descripcion: Busca el valor de una celda.
|#
(define (board-ref bd x y)
  (vector-ref (vector-ref bd y) x))

#|
Nombre : map-board
Parametros : (Natural Natural ValorCelda -> ValorCelda) Tablero -> tablero
Descripcion: Se llama en cada posicion del tablero para producir un valor de celda para esa posicion dando rsultado a un board.
|#
(define (map-board fn bd)
  (build-vector (vector-length bd)
                (lambda (y)
                  (build-vector (vector-length (vector-ref bd y))
                                (lambda (x)
                                  (fn x y (board-ref bd x y)))))))

#|
Nombre : foldr-board
Parametros : (Natural Natural ValorCelda X -> X) X Tablero -> X
Descripcion: Se llama en cada posicion del tablero para producir un valor unico
|#
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

#|
Nombre : place-cell-image
Parametros : Image X Y Image -> Image
Descripcion: Añade una cell-img al tablero en la poscion xy
|#
(define (place-cell-image cell-img x y board-image)
  (place-image cell-img
               (+ (* x CELL-SIZE) (/ CELL-SIZE 2))
               (+ (* y CELL-SIZE) (/ CELL-SIZE 2))
               board-image))

;; Inicia el juego.
(main)
