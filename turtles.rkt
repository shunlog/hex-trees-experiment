#lang racket
(require graphics/value-turtles
         graphics/value-turtles-examples
         threading)

(provide send-off)

;;; the size doesn't matter for turtles-pict
(define t0 (turtles 100 100))


;;; turtles? (-> turtles? turtles?) -> turtles?
;;; Executes (f t) to get a drawing,
;;; but then restores the turtles and pen from original t.
;;; Think of it as creating a clone of the turtle,
;;; sending it off to draw something,
;;; and then forgetting about it.
(define (send-off t f)
  (define color (turtles-pen-color t))
  (define width (turtles-pen-width t))
  (define state (turtle-state t))
  (~> (f t)
      (restore-turtle-state state)
      (set-pen-color color)
      (set-pen-width width)))


(module+ main
    (~> t0
     (send-off (λ~>
                (set-pen-color "red")
                (draw 200 _)
                (set-pen-width 3)
                (regular-poly 6 100 _)))
     (turn -90 _)
     (draw 200 _)
     (set-pen-color "blue")
     (spyro-gyra _)
     turtles-pict))


(define (multiply-spaced n tv f)
  (cond
    [(zero? n) tv]
    [else
     (merge
      (multiply-spaced 
       (sub1 n)
       (f tv)
       f)
      tv)]))

(module+ main
  (~> t0
     (draw 50 _)
     (multiply-spaced 10 _ (λ (t) (turn -3 (draw 10 t))))
     (turn -90 _)
     (set-pen-color "green")
     (draw 100 _)
     (send-off 
      (λ~>
       (set-pen-color "blue")
       (set-pen-width 0.2)
       (multiply-spaced 10 _ (λ (t) (turn 20 (draw 10 t))))))
     (turn -90 _)
     (set-pen-color "red")
     (draw 100 _)
     turtles-pict))


;;; natural? turtle? -> turtle?
;;; draw a tree with n levels of depth
(define (tree n t)
  (define (iter n depth t)
    (define len-scale (/ (random 70 99) 100))
    (define width-scale (/ (random 70 80) 100))
    (define width (* 6 (expt width-scale depth)))
    (define length (* 30 (expt len-scale depth)))
    (define (draw-branch t)
      (draw length t))
    (define (maybe-iter t)
      (define chance
        (* 0.5 (/ n (+ 1 depth))))
      (if (< chance (random))
          t
          (iter (sub1 n) (add1 depth) t)))
    (define angle
      (+ 10 (/ 25 (+ 1 depth))))
    (if (= depth n)
        t
        (~> t
            (set-pen-width width)
            draw-branch
            (turn angle _)
            (send-off (λ (t) (maybe-iter t)))
            (turn (* -2 angle) _)
            (send-off (λ (t) (maybe-iter t))))))
  (send-off t (λ (t) (iter n 0 t))))

(module+ main
  (~> t0
     (turn 90 _)
     (tree 12 _)
     (turn -90 _)
     (move -100 _)
     (draw 200 _)
     turtles-pict))


