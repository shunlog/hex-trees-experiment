#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator)

(define (counter @count action)
  (hpanel
   (button "-" (λ () (action sub1)))
   (text (@count . ~> . number->string))
   (button "+" (λ () (action add1)))))

(define @counter-1 (@ 0))
(define @counter-2 (@ 0))

(render
 (window
  (vpanel
   (counter @counter-1 (λ (proc) (@counter-1 . <~ . proc)))
   (counter @counter-2 (λ (proc) (@counter-2 . <~ . proc))))))
