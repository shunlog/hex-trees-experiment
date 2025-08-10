#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator
         (only-in pict
                  draw-pict
                  scale-to-fit)
         "hex-tree.rkt"
         "turtles.rkt"
         graphics/value-turtles
         (only-in threading [~> ~~>])
         racket/random
         racket/set
         racket/class)


(define (get-pict rand-seed angl start-len start-w scale-len-ls)
  (random-seed rand-seed)
  (when (null? scale-len-ls) (set! scale-len-ls '(0.5)))
  (define hex-tree-instance
    (λ (t)
      (hex-tree
       6 t
       #:angle angl
       #:start-len start-len
       #:start-w start-w
       #:scale-len (λ (old) (* old (random-ref scale-len-ls)))
       #:scale-w (λ (old) (* old 0.6))
       #:terminate? (λ () (< 0.9 (random)))
       #:leaves? #t)))
  (turtles-pict
   (for/fold ([acc (turtles 1 1)])
             ([i (in-range (floor (/ 360 angl 2)))])
     (~~> acc
          (send-off (λ (t) (hex-tree-instance t)))
          (turn (* 2 angl) _)))))


(define throttle-ms 100)
;;; available length scale factors (from which one is picked at random)
(define scale-len-ls0 '(0.5 0.7 1.0))

(define @rand-seed (@ 0))
(define @angle (@ 60))
(define @th-angle (obs-throttle @angle #:duration throttle-ms))
(define @start-len (@ 100))
(define @th-start-len (obs-throttle @start-len #:duration throttle-ms))
(define @start-w (@ 4))
(define @th-start-w (obs-throttle @start-w #:duration throttle-ms))
(define @scale-len (@ (list->set scale-len-ls0)))

(define @scale-len-ls (obs-map @scale-len set->list))

;;; The list of observables should match the arguments
;;; to (get-pict ...)
(define @params
  (obs-combine
   list @rand-seed @th-angle @th-start-len
   @th-start-w @scale-len-ls))

;;; Checkboxes for scale-len
(define checkboxes
  (for/list ([v scale-len-ls0])
    (checkbox
     #:label (number->string v)
     #:checked? #t
     (λ (bool) (<~
                @scale-len
                (λ (s) (if bool
                           (set-add s v)
                           (set-remove s v))))))))

(define (rand-seed) (modulo (current-milliseconds) 100000))

(define output-canvas
  (canvas
   #:margin '(10 10)
   #:style '(resize-corner border)
   @params
   (λ (dc v)
     (define-values (w h) (send dc get-size))
     (define pic (scale-to-fit (apply get-pict v)
                               w h #:mode 'preserve))
     (send dc set-smoothing 'smoothed)
     (draw-pict pic dc 0 0))))

(render
 (window
  #:title "World" #:min-size '(1000 600)
  (hpanel
   (vpanel
    #:stretch '(#f #t)
    (button "Refresh" (λ () (<~ @rand-seed (λ (v) (rand-seed)))))
    (slider (obs-peek @angle) #:min-value 1 #:max-value 120
            (λ (v) (@angle . := . v)))
    (slider (obs-peek @start-len) #:min-value 10 #:max-value 400
            (λ (v) (@start-len . := . v)))
    (slider (obs-peek @start-w) #:min-value 1 #:max-value 10
            (λ (v) (@start-w . := . v)))
    (apply hpanel checkboxes))
   output-canvas)))
