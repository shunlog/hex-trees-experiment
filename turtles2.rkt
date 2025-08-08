#lang racket/base

(require teachpacks/racket-turtle)
(require pict)

;;; This turtle library feel too procedural? idk

;;; It doesn't seem to be possible to read the turtle's position
;;; to implement checkpoints necessary for recursive drawing.
;;; there is (set-origin), but that breaks as soon as you nest them.
;;; I could export the existing (get-position),
;;; but i don't wanna bother with this lib tbh.

;;; This lib feels awkward to use,
;;; the fact that your nested list is flattened
;;; immediately as you pass it to draw...
;;; like bruh, i could've flattened it myself


;;; And i don't like that it uses big-bang,
;;; i just want to generate the picture immediately

;;; And mirroring doesn't work nicely,
;;; i'd rather pass the list of instructions to (mirror ls),
;;; and have it return the same instructions but mirrorred

(define square1
  (list (forward 100)
        (turn-left 90)
        (forward 100)
        (turn-left 90)
        (forward 100)
        (turn-left 90)
        (forward 100)))

(define move
  (list (pen-up)
        (turn-right 90)
        (forward 100)
        (pen-down)
        (change-color "red")))

(define two-squares
  (list square1
        move
        square1))

;; (turtle-pict two-squares)

(define COLORS1 (list "red" "green" "yellow" "purple"))

(define (spiral a x times)
  (if (< times 0)
      '()
      (append (list (forward x)(turn-left a))
              (spiral a (+ x 2)(sub1 times)))))

(define spiral-image
  (list (change-pen-size 2)
        (change-bg-color "black")
        (change-color COLORS1)
        (spiral 91 1 152)))

;; (turtle-pict spiral-image)


(define l (list (forward 100)))
(define beam (list
               (set-origin) l
               (go-to-origin) (turn-left 20) l
               (go-to-origin) (turn-left 20) l
               (go-to-origin) (turn-left 20) l
               ))

(define fly (list
             (set-origin)
             (forward 100)
             beam
             (go-to-origin)
             (turn-left 90)
             (forward 100)
             (mirror-x-on)
             beam))
(turtle-pict fly)
