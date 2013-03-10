#lang racket

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         grace/tools/inferencer)

(provide drracket-buttons)

(define infer-types-bitmap
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc set-pen "black" 1 'transparent)
        (send bdc set-brush "blue" 'solid)
        (send bdc draw-ellipse 2 2 8 8)
        (send bdc set-brush "red" 'solid)
        (send bdc draw-ellipse 6 6 8 8)
        (send bdc set-bitmap #f)
        bmp))

(define (make-inferencer-button)
  (list 
   "Infer Types"
   infer-types-bitmap
   (lambda (frame)
     (start-inference-tool frame))))

(define drracket-buttons
  (list (make-inferencer-button)))