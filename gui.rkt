#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator
         (only-in pict
                  draw-pict
                  scale-to-fit
                  inset)
         "hex-tree.rkt"
         "turtles.rkt"
         graphics/value-turtles
         (only-in threading [~> ~~>])
         racket/random
         racket/set
         racket/class
         (only-in racket/draw make-color))

(define (rand-seed) (modulo (current-milliseconds) 100000))

;;; For some reason using 1, 1 gave really choppy lines
;;; start upwards like a real tree
(define t0 (turn 90 (turtles 1000 1000)))

(define (get-pict rand-seed depth angl start-w scale-len-ls
                  term-chance leaves? color
                  n-roots n-roots-auto)
  (random-seed rand-seed)
  (when (null? scale-len-ls) (set! scale-len-ls '(0.5)))
  (define hex-tree-instance
    (λ (t)
      (hex-tree
       depth t
       #:angle angl
       ;; no need to change this since the output is scaled to fit
       #:start-len 100 
       #:start-w start-w
       #:scale-len (λ (old) (* old (random-ref scale-len-ls)))
       #:scale-w (λ (old) (* old (expt 0.1 (/ 1 depth))))
       #:terminate? (λ () (< (random) term-chance))
       #:leaves? leaves?)))
  (define n-roots1
    (if (not n-roots-auto) n-roots
        (in-range (floor (/ 360 angl 2)))))
  (turtles-pict
   (for/fold ([acc (set-pen-color t0 color)])
             ([i n-roots1])
     (~~> acc
          (send-off (λ (t) (hex-tree-instance t)))
          (turn (* 2 angl) _)))))


;;; available length scale factors (from which one is picked at random)
(define scale-len-ls0 '(0.5 0.7 1.0))
(define scale-len-ls1 '(0.5 1.0))   ; enabled values
;;; label, bg-color, pen-color
(define color-choices
  `(("white" "white" "black")
    ("purple" ,(make-color 76 0 76) ,(make-color 242 229 242))
    ("cyan"  ,(make-color 0 83 83)  ,(make-color 178 220 220))
    ("negative" "black" "white")))

(define @rand-seed (@ 4))
(define @depth (@ 8))
(define @angle (@ 60))
(define @start-w (@ 8))
;;; set of length scale options
(define @scale-len-ls (@ (list->set scale-len-ls1)))
(define @scale-len-custom (@ 0.85))
(define @term-chance (@ 15))
(define @leaves? (@ #f))
(define @color-pen (@ (caddr (car color-choices))))
(define @color-bg (@ (cadr (car color-choices))))
;;; number of roots
(define @n-roots (@ 1))
(define @n-roots-auto (@ #f))


;;; limit callback frequency, especially on slider updates
;;; TODO: throtlle makes the sliders buggy on Macos
(define throttle-ms 10)
;;; The list of observables must match the arguments
;;; to (get-pict ...)
(define @params
  (obs-combine
   list
   @color-bg                            ; must be first
   @rand-seed
   @depth
   @angle
   @start-w
   (obs-combine cons
                @scale-len-custom
                (obs-map @scale-len-ls set->list))
   (obs-map @term-chance (λ (v) (/ v 100)))
   @leaves?
   @color-pen
   @n-roots
   @n-roots-auto))

;;; Checkboxes for scale-len
(define checkboxes
  (for/list ([v scale-len-ls0])
    (checkbox
     #:label (number->string v)
     #:checked? #t
     (λ (bool) (<~
                @scale-len-ls
                (λ (s) (if bool
                           (set-add s v)
                           (set-remove s v))))))))

(define output-canvas
  (canvas
   #:margin '(10 10)
   #:style '(resize-corner border)
   @params
   (λ (dc v)
     (define-values (col-bg args) (values (car v) (cdr v)))
     (define-values (w h) (send dc get-size))
     (define pic (~~> (apply get-pict args)
                      (inset 15)
                      (scale-to-fit w h #:mode 'preserve)))
     (send dc set-background col-bg)
     (send dc clear)
     (send dc set-smoothing 'smoothed)
     (draw-pict pic dc 0 0))))

(render
 (window
  #:title "World" #:min-size '(1000 600)
  (hpanel
   #:alignment '(center top)
   (vpanel
    #:stretch '(#f #f)
    #:alignment '(left top)
    (button "Refresh" (λ () (<~ @rand-seed (λ (v) (rand-seed)))))
    (hpanel
     (text "Depth:")
     (slider @depth #:min-value 1 #:max-value 8
             (λ (v) (@depth . := . v))))

    (hpanel
     (text "Angle:")
     (button "-" #:stretch '(#f #f) (λ () (<~ @angle sub1)))
     (slider @angle #:min-value 1 #:max-value 120
             (λ (v) (@angle . := . v)))
     (button "+" #:stretch '(#f #f) (λ () (<~ @angle add1))))

    (hpanel
     (text "Number of roots:")
     (slider @n-roots #:enabled? (obs-map @n-roots-auto not)
             #:min-value 1 #:max-value 12
             (λ (v) (:= @n-roots v)))
     (checkbox #:checked? @n-roots-auto
               #:label "Auto"
               (λ (bool) (:= @n-roots-auto bool))))

    (hpanel
     (text "Termination chance:")
     (text (obs-map @term-chance
                    (λ (n) (~~> n number->string
                                (string-append "%")))))
     (slider @term-chance
             #:min-value 0 #:max-value 100
             (λ (v) (:= @term-chance v))))

    (hpanel
     (text "Pen width:")
     (slider @start-w #:min-value 1 #:max-value 10
             (λ (v) (:= @start-w v))))

    (hpanel
     (text "Scale options:")
     (input (number->string (obs-peek @scale-len-custom))
            #:stretch '(#f #f)
            (λ (evtype v)
              (if (eq? evtype 'return)
                  (:= @scale-len-custom
                      (string->number v))
                  #f)))
     (apply hpanel checkboxes))
    
    (hpanel
     (text "Draw leaves:")
     (checkbox
      #:checked? @leaves?
      (λ (bool) (<~ @leaves? (λ (v) bool)))))

    ;; label, bg-color, pen-color
    (hpanel
     (text "Colorscheme:")
     (choice color-choices
             #:choice->label car
             (λ (ls)
               (:= @color-bg (cadr ls))
               (:= @color-pen (caddr ls))))))
   
   output-canvas)))
