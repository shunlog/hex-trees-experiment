#lang racket
(require graphics/value-turtles
         graphics/value-turtles-examples
         racket/random
         threading
         "turtles.rkt")

(provide hex-tree)

;;; the size doesn't matter for turtles-pict
(define t0 (turtles 100 100))


;;; draws a branch straight, then recurses
;;; At each recursive step it:
;;; - scales down the length using (scale-len)
;;; - scales down the width using (scale-w)
;;; - terminates if (terminate?) returns #t
(define
  (hex-tree lim t [depth 0] [len-scale 1] [w-scale 1]
            #:angle [ang 60]
            #:start-len [len0 80]
            #:start-w [w0 7]
            #:scale-len
            [scale-len
             (λ (old) (* old (random-ref '(0.5 1))))]
            #:scale-w [scale-w (λ (old) (* old 0.75))]
            #:terminate? [terminate? (λ () (< 0.9 (random)))]
            #:leaves? [leaves? #t])
  (define len (* len0 len-scale))
  (define new-len-scale (scale-len len-scale))
  (define w (* w0 w-scale))
  (define new-w-scale (scale-w w-scale))
  (define (recurse t)
    (hex-tree
     lim t (add1 depth) new-len-scale new-w-scale
     #:angle ang #:start-len len0 #:start-w w0
     #:scale-len scale-len #:scale-w scale-w
     #:terminate? terminate?
     #:leaves? leaves?))
  (define (draw-branch t)
    (~> t
        (set-pen-width w)
        (draw len _)))
  (define (draw-leaf t)
    (define radius (+ 2 (* w-scale 16))) ; through trial and error
    (~> t
        (draw-branch _)           ; give the leaf a stem
        (regular-poly (quotient 360 ang) radius _))
    )
  (if (or (= lim depth) (terminate?))
      (if leaves? (draw-leaf t) t)
      (~> t
          (draw-branch _)
          (send-off
           ;; make the pattern a bit more interesting for angles > 90
           (λ~> (turn (remainder ang 90) _) 
                (recurse)))
          (send-off
           (λ~> (turn (- ang) _)
                (recurse))))))

;;; test defaults
(module+ main
  (~> t0 (hex-tree 6 _) turtles-pict))


;;; draw N trees radially,
;;; where N = 360 / angle / 2
(module+ main
  (let* ([angl 80]
        [hex-tree-instance
         (λ (t)
           (hex-tree
            6 t
            #:angle angl
            #:start-len 100
            #:start-w 3
            #:scale-len (λ (old) (* old (random-ref '(0.6 1))))
            #:scale-w (λ (old) (* old 0.6))
            #:terminate? (λ () (< 0.9 (random)))
            #:leaves? #t))])
    (~>
     (for/fold ([acc t0])
               ([i (in-range (floor (/ 360 angl 2)))])
       (~> acc
           (send-off (λ (t) (hex-tree-instance t)))
           (turn (* 2 angl) _)))
     turtles-pict)))
