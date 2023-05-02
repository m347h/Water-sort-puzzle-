;; A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))

;;; Constants

(define emptygame
  (make-game 0 5
             (list empty empty empty empty empty)))

(define emptygame2
  (make-game 10 3 empty))

(define emptygame3
  (make-game 10 3 (list empty empty)))

(define smallgame1
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallgame2
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallinvalidgame1
  (make-game 2 1
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))


(define smallinvalidgame2
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'blue)
                   (list))))

(define smallinvalidgame3
  (make-game 2 2
             (list (list 'blue 'red 'blue)
                   (list 'red)
                   (list))))


(define smallgamefinal
  (make-game 2 2
             (list (list)
                   (list 'blue 'blue)
                   (list 'red 'red))))


(define mediumgame
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   (list))))

(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

(define largergame
  (make-game 3 3
             (list (list 'blue 'red 'red)
                   (list 'yellow 'blue 'yellow)
                   (list 'red 'yellow 'blue)
                   (list))))

(define biggame
  (make-game 5 3
             (list (list 'blue 'blue 'red 'red 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list)
                   (list))))

(define biggame2
  (make-game 5 3
             (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list)
                   (list 'blue 'blue 'red 'red 'yellow)
                   (list))))

(define biggamesolve
  (make-game 5 3
             (list (list 'blue 'blue 'blue 'blue 'blue)
                   (list 'red 'red 'red 'red 'red)
                   (list 'yellow 'yellow 'yellow 'yellow 'yellow)
                   (list)
                   (list))))

(define hugegame
  (make-game 4 9
             (list (list 'purple 'pink 'yellow 'blue)
                   (list 'blue 'green 'purple 'white)
                   (list 'orange 'yellow 'black 'blue)
                   (list 'white 'orange 'orange 'pink)
                   (list 'pink 'red 'red 'black)
                   (list 'yellow 'green 'orange 'blue)
                   (list 'white 'purple 'red 'yellow)
                   (list 'green 'red 'green 'black)
                   (list 'purple 'black 'white 'pink)
                   (list)
                   (list))
             ))

;; a)

;; (exact-size-times? los s size) consumes a list of Symbols los, a symbol s and a Nat size
;; and produce boolean value true if symbol s appears size times in the list los, and false otherwise.

;;Examples:

(check-expect (exact-size-times? (list 'red 'red 'green 'green 'blue 'blue 'black 'black) 'green 2)
              true)
(check-expect (exact-size-times? (list 'red 'red 'green 'green 'blue 'blue 'black 'black) 'green 1)
              false)

;; exact-size-times?: (listof Sym) Sym Nat -> Bool
(define (exact-size-times? los s size)
  (= (foldr (lambda (curr rror)
              (cond
                [(symbol=? curr s) (add1 rror)]
                [else rror]))
            0
            los)
     size))

;;Tests:
(check-expect (exact-size-times? (list 'red 'red 'green 'green 'blue 'blue 'black 'black) 'green 2)
              true)
(check-expect (exact-size-times? (list 'red 'red 'green 'green 'blue 'blue 'black 'black) 'green 1)
              false)

;; (remove-all-occurrence-of-s los s) consumes a listof Sym los and a Sym s and produces a listof Sym
;; with all occurences of symbol s removed.

;; Examples:
(check-expect (remove-all-occurrence-of-s (list 'red 'red 'green 'green 'blue 'blue 'black 'black)
                                          'red) (list 'green 'green 'blue 'blue 'black 'black))
(check-expect (remove-all-occurrence-of-s (list 'red 'red 'green 'green 'blue 'blue 'black 'black)
                                          'green) (list 'red 'red 'blue 'blue 'black 'black))

;; remove-all-occurrence-of-s: (listof Sym) Sym -> (listof Sym)
(define (remove-all-occurrence-of-s los s)
  (filter (lambda (curr)
            (not (symbol=? curr s)))
          los))

;; Tests:
(check-expect (remove-all-occurrence-of-s (list 'red 'red 'green 'green 'blue 'blue 'black 'black)
                                          'red) (list 'green 'green 'blue 'blue 'black 'black))
(check-expect (remove-all-occurrence-of-s (list 'red 'red 'green 'green 'blue 'blue 'black 'black)
                                          'green) (list 'red 'red 'blue 'blue 'black 'black))

;; (check-colour? size num los) consumes two natural numbers size and num and a list of symbols, los,
;; and produces boolean value true if each symbol in the list appears exactly size times
;; and if there are at most num different symbols, otherwise produce boolean value false.
 
;;Examples: 
(check-expect (check-colour? 2 3 (list 'red 'red 'blue 'blue 'yellow 'yellow)) true)
(check-expect (check-colour? 2 3 (list 'red 'blue 'blue 'yellow 'yellow)) false)
(check-expect (check-colour? 2 3 (list 'red 'red 'green 'green)) true)
(check-expect (check-colour? 2 3 (list 'red 'red 'green 'green 'blue 'blue)) true)
(check-expect (check-colour? 2 3 (list 'red 'green 'green 'blue 'blue)) false)
(check-expect (check-colour? 2 3 (list 'red 'red 'green 'green 'blue 'blue 'black 'black)) false)

;; check-colour?: Nat Nat (listof Sym) -> Bool
(define (check-colour? size num los)
  (cond
    [(empty? los)  
     (cond
       [(>= num 0) true]
       [else false])]
    [(exact-size-times? los (first los) size)
     (check-colour? size (sub1 num) (remove-all-occurrence-of-s los (first los)))]
    [else false]))

;;Tests: 
(check-expect (check-colour? 2 3 (list 'red 'red 'blue 'blue 'yellow 'yellow)) true)
(check-expect (check-colour? 2 3 (list 'red 'blue 'blue 'yellow 'yellow)) false)
(check-expect (check-colour? 2 3 (list 'red 'red 'green 'green)) true)
(check-expect (check-colour? 2 3 (list 'red 'red 'green 'green 'blue 'blue)) true)
(check-expect (check-colour? 2 3 (list 'red 'green 'green 'blue 'blue)) false)
(check-expect (check-colour? 2 3 (list 'red 'red 'green 'green 'blue 'blue 'black 'black)) false)

;;b)

;; (check-tubesize? tubesize tubes) consumes natural number tubesize and listof list of Symbols tubes
;; and produces boolean value true if each tube is atmost tubesize and false otherwise. 

;;Examples:

(check-expect (check-tubesize? 2 (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list)))
true)

(check-expect (check-tubesize? 1 (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list)))
false)

;; check-tubesize?: Nat (listof (listof Sym)) -> Bool 
(define (check-tubesize? tubesize tubes)
  (cond
    [(empty? tubes) true]
    [(<= (length (first tubes)) tubesize) (check-tubesize? tubesize (rest tubes))]
    [else false]))

;;Tests:

(check-expect (check-tubesize? 2 (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list)))
true)

(check-expect (check-tubesize? 1 (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list)))
false)

;; (valid-game? gm) consumes a Game gm and produces a boolean value indicating whether or not the 
;; game is valid (that is if all tubes have at most tubesize symbols in them; there are at most 
;; maxcolours different symbols; and there are exactly tubesize occurrences of each different symbol.

  ;;Examples:
(check-expect (valid-game? hugegame) true)
(check-expect (valid-game? biggame2) true)
(check-expect (valid-game? (make-game 2 3
                                      (list (list 'blue 'red 'yellow 'yellow)
                                            (list 'red 'yellow)
                                            (list 'yellow 'blue)
                                            (list)))) false)
(check-expect (valid-game? emptygame) true)
(check-expect (valid-game? emptygame2) true)
(check-expect (valid-game? emptygame3) true)
(check-expect (valid-game? smallgame1) true)
(check-expect (valid-game? smallgame2) true)
(check-expect (valid-game? smallinvalidgame1) false)
(check-expect (valid-game? smallinvalidgame2) false)
(check-expect (valid-game? smallinvalidgame3) false)
(check-expect (valid-game? smallgamefinal) true)
(check-expect (valid-game? mediumgame) true)
(check-expect (valid-game? mediumgamestuck) true)
(check-expect (valid-game? largergame) true)
(check-expect (valid-game? biggame) true)
(check-expect (valid-game? biggame2) true)
(check-expect (valid-game? biggamesolve) true)
(check-expect (valid-game? hugegame) true)

;; valid-game?: Game -> Bool 
(define (valid-game? gm)
  (and (check-tubesize? (game-tubesize gm) (game-tubes gm))
       (check-colour? (game-tubesize gm)
                      (game-maxcolours gm)
                      (foldr append empty (game-tubes gm)))))
  ;;Tests:
(check-expect (valid-game? hugegame) true)
(check-expect (valid-game? biggame2) true)
(check-expect (valid-game? (make-game 2 3
                                      (list (list 'blue 'red 'yellow 'yellow)
                                            (list 'red 'yellow)
                                            (list 'yellow 'blue)
                                            (list)))) false)
(check-expect (valid-game? emptygame) true)
(check-expect (valid-game? emptygame2) true)
(check-expect (valid-game? emptygame3) true)
(check-expect (valid-game? smallgame1) true)
(check-expect (valid-game? smallgame2) true)
(check-expect (valid-game? smallinvalidgame1) false)
(check-expect (valid-game? smallinvalidgame2) false)
(check-expect (valid-game? smallinvalidgame3) false)
(check-expect (valid-game? smallgamefinal) true)
(check-expect (valid-game? mediumgame) true)
(check-expect (valid-game? mediumgamestuck) true)
(check-expect (valid-game? largergame) true)
(check-expect (valid-game? biggame) true)
(check-expect (valid-game? biggame2) true)
(check-expect (valid-game? biggamesolve) true)
(check-expect (valid-game? hugegame) true)
;;c)

;; (is-full? tube tubesize) consumes a list of symbols, tube, and a natural number, tubesize,
;; and produces the boolean value true if the tube is full, that is when the length of the tube
;; equals the tubesize, and false otherwise. 

;;Examples:
(check-expect (is-full? (list 'red 'red 'green 'green 'blue 'blue 'black 'black) 8) true)
(check-expect (is-full? (list 'red 'red 'green 'green 'blue 'blue 'black 'black) 9) false)

;;is-full?: (listof Sym) Nat -> Bool 
(define (is-full? tube tubesize)
  (= (length tube) tubesize))

;;Tests:
(check-expect (is-full? (list 'red 'red 'green 'green 'blue 'blue 'black 'black) 8) true)
(check-expect (is-full? (list 'red 'red 'green 'green 'blue 'blue 'black 'black) 9) false)

;; (all-same-color? tube) consumes a list of symbols tube and produce boolean value true if the
;; symbols in the tube are all of the same colour and false otherwise.

;;Examples:

(check-expect (all-same-color? (list 'red 'red 'green 'green 'blue 'blue 'black 'black)) false)
(check-expect (all-same-color? (list 'red 'red)) true)

;; all-same-color?: (listof Sym) -> Bool 
(define (all-same-color? tube)
  (cond
    [(empty? tube) true]
    [(empty? (rest tube)) true]
    [(symbol=? (first tube) (second tube)) (all-same-color? (rest tube))]
    [else false]))

;;Tests:

(check-expect (all-same-color? (list 'red 'red 'green 'green 'blue 'blue 'black 'black)) false)
(check-expect (all-same-color? (list 'red 'red)) true)

;; (count-different-colours los) consumes a list of symbols, los, and produces a natural number
;; that indicates the number of colours in the list. 

;;Examples:
(check-expect (count-different-colours (list 'red 'red 'green 'green 'blue 'blue 'black 'black)) 4)
(check-expect (count-different-colours (list 'red 'red)) 1)

;;count-different-colours: (listof Sym) -> Nat

(define (count-different-colours los)
  (local
    [(define (count-different-colours/acc los res)
       (cond
         [(empty? los) res]
         [else (count-different-colours/acc (remove-all-occurrence-of-s los (first los))
                                            (+ 1 res))]))]
    (count-different-colours/acc los 0)))

;;Tests:
(check-expect (count-different-colours (list 'red 'red 'green 'green 'blue 'blue 'black 'black)) 4)
(check-expect (count-different-colours (list 'red 'red)) 1)

;; (remove-completed-tubes tubes tubesize) consumes a list of list of symbols, tubes, and a
;; natural number, tubesize, and produces a list of list of symbols with the completed tubes removed. 

;;Examples:
(check-expect (remove-completed-tubes (list (list 'yellow 'yellow) (list 'blue 'yellow)) 2)
(list (list 'blue 'yellow)))
(check-expect (remove-completed-tubes (list (list 'yellow 'yellow) (list 'blue 'yellow) '()) 2)
(list (list 'blue 'yellow) '()))

;; remove-completed-tubes: (listof (listof Sym)) Nat -> (listof (listof Sym))
(define (remove-completed-tubes tubes tubesize)
  (cond
    [(empty? tubes) empty]
    [(and (is-full? (first tubes) tubesize)
          (all-same-color? (first tubes)))
     (remove-completed-tubes (rest tubes) tubesize)]
    [else (cons (first tubes) (remove-completed-tubes (rest tubes) tubesize))]))

;;Tests:
(check-expect (remove-completed-tubes (list (list 'yellow 'yellow) (list 'blue 'yellow)) 2)
(list (list 'blue 'yellow)))
(check-expect (remove-completed-tubes (list (list 'yellow 'yellow) (list 'blue 'yellow) '()) 2)
(list (list 'blue 'yellow) '()))

;; (remove-completed gm) consumes a Game, gm, and produces a Game with the completed tubes removed. 

;;Examples:
(check-expect (remove-completed smallgamefinal)
              (make-game 2 0
                         (list (list))))

(check-expect (remove-completed biggame2)
              biggame2)

(check-expect (remove-completed (make-game 4 9
                                           (list (list 'purple 'pink 'yellow 'blue)
                                                 (list 'green 'green 'green 'green)
                                                 (list 'orange 'yellow 'black 'blue)
                                                 (list 'white 'orange 'orange 'pink)
                                                 (list 'pink 'red 'red 'black)
                                                 (list 'yellow 'purple 'orange 'blue)
                                                 (list 'white 'purple 'red 'yellow)
                                                 (list 'blue 'red 'white 'black)
                                                 (list 'purple 'black 'white 'pink)
                                                 (list)
                                                 (list))))
              (make-game 4 8
                         (list (list 'purple 'pink 'yellow 'blue)
                               (list 'orange 'yellow 'black 'blue)
                               (list 'white 'orange 'orange 'pink)
                               (list 'pink 'red 'red 'black)
                               (list 'yellow 'purple 'orange 'blue)
                               (list 'white 'purple 'red 'yellow)
                               (list 'blue 'red 'white 'black)
                               (list 'purple 'black 'white 'pink)
                               (list)
                               (list))))

(check-expect (remove-completed (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'yellow 'yellow)
                                                 (list 'red 'blue)
                                                 (list))))
              (make-game 2 2
                         (list (list 'blue 'red)
                               (list 'red 'blue)
                               (list)))) 

;;remove-completed: Game -> Game 
(define (remove-completed gm)
  (local
    [(define new-tubes (remove-completed-tubes (game-tubes gm) (game-tubesize gm)))]
    (make-game (game-tubesize gm)
               (count-different-colours (foldr append empty new-tubes))
               new-tubes)))

;;Tests: 
(check-expect (remove-completed smallgamefinal)
              (make-game 2 0
                         (list (list))))

(check-expect (remove-completed biggame2)
              biggame2)

(check-expect (remove-completed (make-game 4 9
                                           (list (list 'purple 'pink 'yellow 'blue)
                                                 (list 'green 'green 'green 'green)
                                                 (list 'orange 'yellow 'black 'blue)
                                                 (list 'white 'orange 'orange 'pink)
                                                 (list 'pink 'red 'red 'black)
                                                 (list 'yellow 'purple 'orange 'blue)
                                                 (list 'white 'purple 'red 'yellow)
                                                 (list 'blue 'red 'white 'black)
                                                 (list 'purple 'black 'white 'pink)
                                                 (list)
                                                 (list))))
              (make-game 4 8
                         (list (list 'purple 'pink 'yellow 'blue)
                               (list 'orange 'yellow 'black 'blue)
                               (list 'white 'orange 'orange 'pink)
                               (list 'pink 'red 'red 'black)
                               (list 'yellow 'purple 'orange 'blue)
                               (list 'white 'purple 'red 'yellow)
                               (list 'blue 'red 'white 'black)
                               (list 'purple 'black 'white 'pink)
                               (list)
                               (list))))

(check-expect (remove-completed (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'yellow 'yellow)
                                                 (list 'red 'blue)
                                                 (list))))
              (make-game 2 2
                         (list (list 'blue 'red)
                               (list 'red 'blue)
                               (list)))) 

;; d)

;; (finished-game? gm) consumes a Game, gm, and produces Boolean value, true, if the game is finished
;; (that is all tubes are either empty, or each tube is full with all balls of the same colour),
;; and produces boolean value, false, otherwise. 

;;Examples:  
(check-expect (finished-game? smallgamefinal) true)
(check-expect (finished-game? biggame2) false)
(check-expect (finished-game? emptygame) true)
(check-expect (finished-game? emptygame2) true)
(check-expect (finished-game? hugegame) false)

;; finished-game?: Game -> Bool
(define (finished-game? gm)
  (empty? (filter (lambda (tube)
                    (not (empty? tube)))
                  (game-tubes (remove-completed gm)))))
  
;;Tests: 
(check-expect (finished-game? smallgamefinal) true)
(check-expect (finished-game? biggame2) false)
(check-expect (finished-game? emptygame) true)
(check-expect (finished-game? emptygame2) true)
(check-expect (finished-game? hugegame) false)

;;e)

;; (calculate-block los) consumes a list of symbols, los, and produces a natural number that 
;; indicates the number of blocks of consecutive colours in the list los.

;;Examples:

(check-expect (calculate-block (list 'blue 'red)) 2)
(check-expect (calculate-block '()) 0)
(check-expect (calculate-block (list 'blue 'blue 'red)) 2)

;; calculate-block: (listof Sym) -> Nat
(define (calculate-block los)
  (cond
    [(empty? los) 0]
    [else (local
            [(define (calculate-block/acc los last-color acc)
               (cond
                 [(empty? los) acc]
                 [(symbol=? (first los) last-color) (calculate-block/acc (rest los) last-color acc)]
                 [else (calculate-block/acc (rest los) (first los) (add1 acc))]))]
            (calculate-block/acc (rest los) (first los) 1))]))

;;Tests:

(check-expect (calculate-block (list 'blue 'red)) 2)
(check-expect (calculate-block '()) 0)
(check-expect (calculate-block (list 'blue 'blue 'red)) 2)

;; (num-blocks llos) consumes a list of list of symbols, llos, and produces a natural number that
;; indicates the number of blocks in the list of list of symbols llos.

;;Examples:
(check-expect (num-blocks (list empty (list 'blue 'blue 'red) (list 'red 'red 'red))) 3)
(check-expect (num-blocks (list empty (list 'blue 'blue 'red 'blue) (list 'red 'red 'red))) 4)

;; num-blocks: (listof (listof Sym)) -> Nat 
(define (num-blocks llos)
  (foldr (lambda (los rror)
           (+ (calculate-block los) rror))
         0
         llos))

;;Tests:
(check-expect (num-blocks (list empty (list 'blue 'blue 'red) (list 'red 'red 'red))) 3)
(check-expect (num-blocks (list empty (list 'blue 'blue 'red 'blue) (list 'red 'red 'red))) 4)

;;f)

;; (remove-tube-from-tubes tubes target-tube) consumes a list of list of symbols, tubes, and a list 
;; of symbols, target-tube, and produces a list of list of symbols with the list of symbols 
;; target-tube removed from it.

;; Examples: 
(check-expect (remove-tube-from-tubes (list (list 'blue 'blue 'red) (list 'red 'red 'red))
                                      (list 'red 'red 'red))(list (list 'blue 'blue 'red)))

(check-expect (remove-tube-from-tubes (list (list 'blue 'blue 'red)(list 'red 'red 'red)
                                            (list 'blue 'red 'red)) (list 'blue 'red 'red))
(list (list 'blue 'blue 'red) (list 'red 'red 'red)))

;; remove-tube-from-tubes: (listof (listof Sym)) (listof Sym) -> (listof (listof Sym))
  (define (remove-tube-from-tubes tubes target-tube)
  (cond
    [(equal? (first tubes) target-tube) (rest tubes)]
    [else (cons (first tubes) (remove-tube-from-tubes (rest tubes) target-tube))]))

;; Tests:
(check-expect (remove-tube-from-tubes (list (list 'blue 'blue 'red) (list 'red 'red 'red))
                                      (list 'red 'red 'red))(list (list 'blue 'blue 'red)))

(check-expect (remove-tube-from-tubes (list (list 'blue 'blue 'red)(list 'red 'red 'red)
                                            (list 'blue 'red 'red)) (list 'blue 'red 'red))
(list (list 'blue 'blue 'red) (list 'red 'red 'red)))

;; (same-tubes tubes1 tubes2) consumes two lists of (listof Sym); tubes1 and tubes2 and produces
;;  boolean value true if the tubes contain identical balls in identical order and false otherwise.

;; same-tubes: (listof (listof Sym)) (listof (listof Sym)) -> Bool 
(define (same-tubes tubes1 tubes2)
  (cond
    [(empty? tubes1) true]
    [(member? (first tubes1) tubes2)
     (same-tubes (rest tubes1) (remove-tube-from-tubes tubes2 (first tubes1)))]
[else false]))

;; (equiv-game? gm1 gm2) consumes Game, gm1, and Game, gm2, and produces boolean value true if
;; gm1 and gm2 are equivalent, and false otherwise.

;; Examples:
(check-expect (equiv-game? (make-game 2 3
                                       (list (list 'blue 'red)
                                             (list 'blue 'red)
                                             (list))) (make-game 2 3
                                                                 (list (list 'blue 'red)
                                                                       (list 'red 'yellow)
                                                                       (list 'yellow 'blue)
                                                                       (list))))false) 

(check-expect (equiv-game? (make-game 5 3
                                      (list (list 'blue 'blue 'red 'red 'yellow)
                                            (list 'red 'red 'yellow 'blue 'red)
                                            (list 'yellow 'blue 'blue 'yellow 'yellow)
                                            (list)
                                            (list)))
                           (make-game 5 3
                                      (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                            (list 'red 'red 'yellow 'blue 'red)
                                            (list)
                                            (list 'blue 'blue 'red 'red 'yellow)
                                            (list))))
              true)

;; equiv-game?: Game Game -> Bool 
  
(define (equiv-game? gm1 gm2)
  (and (= (game-maxcolours gm1) (game-maxcolours gm2))
       (= (game-tubesize gm1) (game-tubesize gm2))
       (= (length (game-tubes gm1)) (length (game-tubes gm2)))
       (same-tubes (game-tubes gm1) (game-tubes gm2))))

;; Tests: 

(check-expect (equiv-game? (make-game 2 3
                                       (list (list 'blue 'red)
                                             (list 'blue 'red)
                                             (list))) (make-game 2 3
                                                                 (list (list 'blue 'red)
                                                                       (list 'red 'yellow)
                                                                       (list 'yellow 'blue)
                                                                       (list))))false) 

(check-expect (equiv-game? (make-game 5 3
                                      (list (list 'blue 'blue 'red 'red 'yellow)
                                            (list 'red 'red 'yellow 'blue 'red)
                                            (list 'yellow 'blue 'blue 'yellow 'yellow)
                                            (list)
                                            (list)))
                           (make-game 5 3
                                      (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                            (list 'red 'red 'yellow 'blue 'red)
                                            (list)
                                            (list 'blue 'blue 'red 'red 'yellow)
                                            (list))))
              true)


;;g) 

;; (all-equiv-logx-logy? logx logy) consumes two lists of Games, logx and logy, and produce boolean
;; value true if every game in logx has one equivalent game in logy, and otherwise produces false. 

;; Examples:

(check-expect (all-equiv-logx-logy? (list (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list))) (make-game 5 3
                                    (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                         (list 'red 'red 'yellow 'blue 'red)
                                               (list)
                                                              (list 'blue 'blue 'red 'red 'yellow)
                                                               (list)))) (list (make-game 2 3
                                                          (list (list 'blue 'red)
                                                                            (list 'blue 'red)
                                                                      (list))) (make-game 5 3
                                               (list (list 'blue 'blue 'red 'red 'yellow)
                                                 (list 'red 'red 'yellow 'blue 'red)
                                               (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                                                      (list)
                                                                                    (list)))))
true)

(check-expect  (all-equiv-logx-logy? (list (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list))) (make-game 5 3
                                                     (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                          (list 'red 'red 'yellow 'blue 'red)
                                                            (list)
                                                            (list 'blue 'blue 'red 'red 'yellow)
                                                        (list)))) (list (make-game 2 3
                                                                   (list (list 'blue 'red)
                                                                   (list 'red 'yellow)
                                                                   (list 'yellow 'blue)
                                                                   (list))) (make-game 5 3
                                                           (list (list 'blue 'blue 'red 'red 'yellow)
                                                               (list 'red 'red 'yellow 'blue 'red)
                                                           (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                          (list)
                                                               (list)))))
false)

;; all-equiv-logx-logy?: (listof Game) (listof Game) -> Bool 
(define (all-equiv-logx-logy? logx logy)
  (cond
    [(empty? logx) true]
    [else (and (= (length (filter (lambda (curr-gm-from-logy)
                                    (equiv-game? (first logx) curr-gm-from-logy))
                                  logy))
                  1)
               (all-equiv-logx-logy? (rest logx) logy))]))

;; Tests:

(check-expect (all-equiv-logx-logy? (list (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list))) (make-game 5 3
                                    (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                         (list 'red 'red 'yellow 'blue 'red)
                                               (list)
                                                              (list 'blue 'blue 'red 'red 'yellow)
                                                               (list)))) (list (make-game 2 3
                                                          (list (list 'blue 'red)
                                                                            (list 'blue 'red)
                                                                      (list))) (make-game 5 3
                                               (list (list 'blue 'blue 'red 'red 'yellow)
                                                 (list 'red 'red 'yellow 'blue 'red)
                                               (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                                                      (list)
                                                                                    (list)))))
true)

(check-expect  (all-equiv-logx-logy? (list (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list))) (make-game 5 3
                                                     (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                          (list 'red 'red 'yellow 'blue 'red)
                                                            (list)
                                                            (list 'blue 'blue 'red 'red 'yellow)
                                                        (list)))) (list (make-game 2 3
                                                                   (list (list 'blue 'red)
                                                                   (list 'red 'yellow)
                                                                   (list 'yellow 'blue)
                                                                   (list))) (make-game 5 3
                                                           (list (list 'blue 'blue 'red 'red 'yellow)
                                                               (list 'red 'red 'yellow 'blue 'red)
                                                           (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                          (list)
                                                               (list)))))
false)

;; (all-equiv? log1 log2) consumes two lists of Games, log1 and log2, and produce boolean value true 
;; if every game in log1 has one equivalent game in log2, and every game in log2 has one equivalent
;; game in log1, and otherwise produces false.

;;Examples:
(check-expect (all-equiv? (list (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list))) (make-game 5 3
                                    (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                         (list 'red 'red 'yellow 'blue 'red)
                                               (list)
                                                              (list 'blue 'blue 'red 'red 'yellow)
                                                               (list)))) (list (make-game 2 3
                                                          (list (list 'blue 'red)
                                                                            (list 'blue 'red)
                                                                      (list))) (make-game 5 3
                                               (list (list 'blue 'blue 'red 'red 'yellow)
                                                 (list 'red 'red 'yellow 'blue 'red)
                                               (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                                                      (list)
                                                                                    (list))))) true)
(check-expect (all-equiv? (list (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list))) (make-game 5 3
                                                     (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                          (list 'red 'red 'yellow 'blue 'red)
                                                            (list)
                                                            (list 'blue 'blue 'red 'red 'yellow)
                                                        (list)))) (list (make-game 2 3
                                                                   (list (list 'blue 'red)
                                                                   (list 'red 'yellow)
                                                                   (list 'yellow 'blue)
                                                                   (list))) (make-game 5 3
                                                           (list (list 'blue 'blue 'red 'red 'yellow)
                                                               (list 'red 'red 'yellow 'blue 'red)
                                                           (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                          (list)
                                                               (list))))) false)


;; all-equiv?: (listof Game) (listof Game) -> Bool

(define (all-equiv? log1 log2)
  (and (all-equiv-logx-logy? log1 log2)
       (all-equiv-logx-logy? log2 log1)))

;;Tests: 
(check-expect (all-equiv? (list (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list))) (make-game 5 3
                                    (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                         (list 'red 'red 'yellow 'blue 'red)
                                               (list)
                                                              (list 'blue 'blue 'red 'red 'yellow)
                                                               (list)))) (list (make-game 2 3
                                                          (list (list 'blue 'red)
                                                                            (list 'blue 'red)
                                                                      (list))) (make-game 5 3
                                               (list (list 'blue 'blue 'red 'red 'yellow)
                                                 (list 'red 'red 'yellow 'blue 'red)
                                               (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                                                      (list)
                                                                                    (list))))) true)
(check-expect (all-equiv? (list (make-game 2 3
                                           (list (list 'blue 'red)
                                                 (list 'blue 'red)
                                                 (list))) (make-game 5 3
                                                     (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                          (list 'red 'red 'yellow 'blue 'red)
                                                            (list)
                                                            (list 'blue 'blue 'red 'red 'yellow)
                                                        (list)))) (list (make-game 2 3
                                                                   (list (list 'blue 'red)
                                                                   (list 'red 'yellow)
                                                                   (list 'yellow 'blue)
                                                                   (list))) (make-game 5 3
                                                           (list (list 'blue 'blue 'red 'red 'yellow)
                                                               (list 'red 'red 'yellow 'blue 'red)
                                                           (list 'yellow 'blue 'blue 'yellow 'yellow)
                                                          (list)
                                                               (list))))) false)


(check-expect (all-equiv?
               (list
                (make-game 5 3
                           (list (list 'blue 'blue 'red 'red 'yellow)
                                 (list 'red 'red 'yellow 'blue 'red)
                                 (list 'yellow 'blue 'blue 'yellow 'yellow)
                                 (list)
                                 (list)))
                (make-game 4 8
                           (list (list 'purple 'pink 'yellow 'blue)
                                 (list 'orange 'yellow 'black 'blue)
                                 (list 'white 'orange 'orange 'pink)
                                 (list 'pink 'red 'red 'black)
                                 (list 'yellow 'purple 'orange 'blue)
                                 (list 'white 'purple 'red 'yellow)
                                 (list 'blue 'red 'white 'black)
                                 (list 'purple 'black 'white 'pink)
                                 (list)
                                 (list))))
               (list
                (make-game 4 8
                           (list (list 'pink 'red 'red 'black)
                                 (list 'orange 'yellow 'black 'blue)
                                 (list 'white 'orange 'orange 'pink)
                                 (list 'purple 'pink 'yellow 'blue)
                                 (list 'yellow 'purple 'orange 'blue)
                                 (list 'white 'purple 'red 'yellow)
                                 (list 'blue 'red 'white 'black)
                                 (list 'purple 'black 'white 'pink)
                                 (list)
                                 (list)))
                (make-game 5 3
                           (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                 (list 'red 'red 'yellow 'blue 'red)
                                 (list)
                                 (list 'blue 'blue 'red 'red 'yellow)
                                 (list)))))
              true)

(check-expect (all-equiv?
               (list
                (make-game 5 3
                           (list (list 'blue 'blue 'red 'red 'yellow)
                                 (list 'red 'red 'yellow 'blue 'red)
                                 (list 'yellow 'blue 'blue 'yellow 'yellow)
                                 (list)
                                 (list)))
                (make-game 4 8
                           (list (list 'purple 'pink 'yellow 'blue)
                                 (list 'orange 'yellow 'black 'blue)
                                 (list 'white 'orange 'orange 'pink)
                                 (list 'pink 'red 'red 'black)
                                 (list 'yellow 'purple 'orange 'blue)
                                 (list 'white 'purple 'red 'yellow)
                                 (list 'blue 'red 'white 'black)
                                 (list 'purple 'black 'white 'pink)
                                 (list)
                                 (list))))
               (list
                (make-game 4 8
                           (list (list 'pink 'red 'red 'black)
                                 (list 'orange 'yellow 'black 'blue)
                                 (list 'white 'orange 'orange 'pink)
                                 (list 'purple 'pink 'yellow 'blue)
                                 (list 'yellow 'purple 'orange 'blue)
                                 (list 'white 'purple 'red 'yellow)
                                 (list 'blue 'red 'white 'black)
                                 (list 'purple 'black 'white 'pink)
                                 (list)
                                 (list)))
                (make-game 5 3
                           (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                                 (list 'yellow 'red 'red 'blue 'red)
                                 (list)
                                 (list 'blue 'blue 'red 'red 'yellow)
                                 (list)))))
              false)


;; h)

;; (next-games/tube tubes top-color ignore-index add-index tubesize maxcolours) consumes a
; list of list of symbols tubes, a natural number top-color, a natural number ignore-index,
;; a natural number add-index, a natural number tubesize, and a natural number maxcolours to
;; produce a list of symbols. 

;;Examples:

(check-expect (next-games/tube (list (list 'yellow 'yellow) (list 'blue 'yellow)) 'yellow 0 1 2 2)
'())

;; next-games/tube: (listof (listof Sym)) Sym Nat Nat Nat Nat -> (list of Sym) 
(define (next-games/tube tubes top-color ignore-index add-index tubesize maxcolours)
  (cond
    [(= add-index (length tubes)) empty]
    [(or (= add-index ignore-index)
         (= (length (list-ref tubes add-index)) tubesize))
     (next-games/tube tubes top-color ignore-index (add1 add-index) tubesize maxcolours)]
    [else (cons (make-game tubesize maxcolours
                           (build-list (length tubes)
                                       (lambda (index)
                                         (cond
                                           [(= index ignore-index) (rest (list-ref tubes index))]
                                    [(= index add-index) (cons top-color (list-ref tubes index))]
                                           [else (list-ref tubes index)]))))
            (next-games/tube tubes top-color ignore-index (add1 add-index) tubesize maxcolours))]))

;;Tests:

(check-expect (next-games/tube (list (list 'yellow 'yellow) (list 'blue 'yellow)) 'yellow 0 1 2 2)
'())

;; (next-games/tubes curr-index res tubes tubesize maxcolors) consumes a Nat, curr-index,
;; a listof Sym, res, a Nat, tubes, a Nat, tubesize, and a Nat, maxcolors to produce
;; a list of list of symbols as the possible next step choice applying helper function
;; (next-games/tube tubes top-color ignore-index add-index tubesize maxcolours)
;; on the list of symbols produced. 

;;Examples:
 (check-expect (next-games/tubes 0 (list (list 'blue 'red) (list 'blue 'red))
                                 (list (list 'blue 'red) (list 'blue 'red)) 2 2)
(list (list 'blue 'red) (list 'blue 'red)))

 (check-expect (next-games/tubes 0 (list (list 'yellow 'yellow) (list 'blue 'yellow))
                                 (list (list 'yellow 'yellow) (list 'blue 'yellow)) 2 2)
(list (list 'yellow 'yellow) (list 'blue 'yellow)))
 
;; next-games/tubes: Nat (listof (listof Sym)) (listof (listof Sym)) Nat Nat -> (listof (listof Sym)) 
(define (next-games/tubes curr-index res tubes tubesize maxcolors)
  (cond
    [(= curr-index (length tubes)) res]
    [(empty? (list-ref tubes curr-index))
     (next-games/tubes (add1 curr-index)
                       res
                       tubes
                       tubesize
                       maxcolors)]
    [else (next-games/tubes (add1 curr-index)
                            (append (next-games/tube tubes (first (list-ref tubes curr-index))
                                                     curr-index 0 tubesize maxcolors) res)
                            tubes
                            tubesize
                            maxcolors)]))

;; (next-games gm) consumes a Game, gm and produces a list of Games containing the possible games for
;;  the next step. 

;; Examples:

(define (test-next-games gm expected) (all-equiv? (next-games gm) expected))

(check-expect (test-next-games (make-game 2 2 (list (list 'blue 'red) (list 'blue 'red) empty))
                            (list (make-game 2 2 (list (list 'blue 'red) (list 'red) (list 'blue)))
                    (make-game 2 2 (list (list 'red) (list 'blue 'red) (list 'blue)))
                    )) true)


(check-expect (test-next-games (make-game 2 2 (list (list 'red) (list 'blue 'red) (list 'blue)))
                               (list (make-game 2 2 (list (list 'blue 'red) (list 'blue 'red) empty))
                    (make-game 2 2 (list (list 'blue 'red) (list 'red) (list 'blue)))
                    (make-game 2 2 (list (list 'red) (list 'red) (list 'blue 'blue)))
                    (make-game 2 2 (list empty (list 'blue 'red) (list 'red 'blue))))) true)

;; next-games: Game -> (listof Game) 
(define (next-games gm)
  (next-games/tubes 0 empty (game-tubes gm) (game-tubesize gm) (game-maxcolours gm)))

;; Tests:

(check-expect (test-next-games (make-game 2 2 (list (list 'blue 'red) (list 'blue 'red) empty))
                             (list (make-game 2 2 (list (list 'blue 'red) (list 'red) (list 'blue)))
                    (make-game 2 2 (list (list 'red) (list 'blue 'red) (list 'blue)))
                    )) true)


(check-expect (test-next-games (make-game 2 2 (list (list 'red) (list 'blue 'red) (list 'blue)))
                              (list (make-game 2 2 (list (list 'blue 'red) (list 'blue 'red) empty))
                    (make-game 2 2 (list (list 'blue 'red) (list 'red) (list 'blue)))
                    (make-game 2 2 (list (list 'red) (list 'red) (list 'blue 'blue)))
                    (make-game 2 2 (list empty (list 'blue 'red) (list 'red 'blue))))) true)


;; Examples:
;; students should provide some here, or just in tests

;; solve: Game (anyof 'off 'norm 'slow 'fast) -> Bool

;;(define (solve gm draw-option)
  ;;(local
    ;;[(define setup (puzzle-setup gm draw-option))                
     ;;(define (solve-helper to-visit visited)
       ;;(cond
         ;;[(empty? to-visit) false]
     ;;    [else
       ;;   (local
         ;;   [(define draw (draw-board (first to-visit) draw-option))] 
           ;; (cond
             ;; [(finished-game? (first to-visit)) true]
           ;;   [(member? (first to-visit) visited)
             ;;  (solve-helper (rest to-visit) visited)]
             ;; [else
               ;;(local [(define nbrs (next-games (first to-visit)))
                 ;;      (define new (filter (lambda (x) (not (member? x visited))) nbrs))
                   ;;    (define new-to-visit (append new (rest to-visit)))
                     ;;  (define new-visited (cons (first to-visit) visited))]
                ;; (solve-helper new-to-visit new-visited))]))]))]
  ;;  (solve-helper (list gm) empty)))

;; Test cases that can be uncommented as the solution is completed
;(check-expect (solve smallgame1 'slow) true)
;(check-expect (solve mediumgamestuck 'slow) false)

;; Below is the format for testing and timing the solution:
;; be sure to remove any other check-expects when measuring your timing

;(check-expect (time (solve mediumgame 'off)) true)
;(check-expect (time (solve largergame 'off)) true)
;(check-expect (time (solve biggame 'off)) true)
;(check-expect (time (solve hugegame 'off)) true)
;(solve largergame 'fast)
