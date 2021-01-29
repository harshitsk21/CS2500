;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct eyemouthhair (eye mouth hair))

;eyemouthhair is a (make-eyemouthhair String String String)
;Intepretation: is all needed information for the function draw-image
; - eye: is the description of the eye
; - hair: is the description of the hair
; - mouth: is the description of the mouth

(define EMH-1
  (make-eyemouthhair "avatarstate-eyes" "shook-mouth" "aang-hair"))
(define EMH-2
  (make-eyemouthhair "normal-eyes" "frown" "mohawk"))

(define (eyemouthhair-temp EMH)
  (... (eyemouthhair-eye EMH)...
       (eyemouthhair-mouth EMH)...
       (eyemouthhair-hair EMH)...
       ))


(define face (circle 200 "outline" "black"))

;hair:

(define aang-hair (above (rectangle 50 80 "solid" "navy")
                         (isosceles-triangle 100 270 "solid" "navy")))

(define mohawk (triangle 100 "solid" "black"))

(define petal (put-pinhole
               20 20
               (ellipse 20 80 "solid" "purple")))

(define flowery-hair (clear-pinhole
                      (overlay/pinhole
                       (circle 20 "solid" "yellow")
                       (rotate (* 60 0) petal)
                       (rotate (* 60 1) petal)
                       (rotate (* 60 2) petal)
                       (rotate (* 60 3) petal)
                       (rotate (* 60 4) petal)
                       (rotate (* 60 5) petal))))

;eyes:

(define avatarstate-eyes(underlay/offset (circle 30 "solid" "navy")
                                         -125 0
                                         (circle 30 "solid" "navy")))


(define normal-eyes(underlay/offset
                    (overlay
                     (circle 10 "solid" "black")
                     (circle 30 "outline" "black"))
                    -125 0
                    (overlay (circle 10 "solid" "black")
                             (circle 30 "outline" "black"))))


(define winking (underlay/offset (overlay (circle 10 "solid" "black")
                                          (circle 30 "outline" "black"))
                                 -125 0
                                 (line 50 0 "black")))
;mouth

(define shook-mouth (ellipse 120 60 "solid" "black"))

(define smile (crop/align "center" "bottom" 300 40
                          (ellipse 200 90 "outline" "black")))

(define frown (crop/align "center" "top" 300 40
                          (ellipse 200 90 "outline" "black")))



;bit-face: String String String -> Image
; Draws a face with hair, mouth and eyes,
;each of which can be changed by keys h, m, e respectivlly

(define (bit-face emh)
  (big-bang emh
    [to-draw  draw-face]
    [on-key change-face]
    ))


;draw-eye: String -> Image
;Draws a circles with eyes

(check-expect (draw-eye EMH-2) (underlay/offset face
                                                0 -10
                                                normal-eyes))
 
(define (draw-eye EMH)
  (cond
    [(string=? (eyemouthhair-eye EMH) "avatarstate-eyes")
     (underlay/offset face
                      0 -10
                      avatarstate-eyes)]
    [(string=? (eyemouthhair-eye EMH) "normal-eyes")
     (underlay/offset face
                      0 -10
                      normal-eyes)]
    [(string=? (eyemouthhair-eye EMH) "winking")
     (underlay/offset face
                      0 -10
                      winking)]
    ))
;draw-hair; String String-> Image
;draws a face with hair and eyes

(check-expect (draw-hair EMH-1)
              (overlay/align "middle" "top" aang-hair
                             (underlay/offset face
                                              0 -10
                                              avatarstate-eyes)))
 
(define (draw-hair EMH)
  (cond
    [(string=? (eyemouthhair-hair EMH) "aang-hair")
     (overlay/align "middle" "top" aang-hair (draw-eye EMH))]
    [(string=? (eyemouthhair-hair EMH) "flowery-hair")
     (overlay/offset flowery-hair
                     100 180 (overlay/offset flowery-hair 
                                             -100 200
                                             (draw-eye EMH)))]
    [(string=? (eyemouthhair-hair EMH) "mohawk")
     (above mohawk (draw-eye EMH))]
    ))

;draw-face: String String String -> Image
;Draws a face with mouth hair and eyes

(check-expect (draw-face EMH-2)
              (underlay/offset (above mohawk (underlay/offset face
                                                              0 -10
                                                              normal-eyes)) 0 100 frown))

(define (draw-face EMH)
  (cond
    [(string=? (eyemouthhair-mouth EMH) "shook-mouth")
     (underlay/offset (draw-hair EMH)
                      0 100
                      shook-mouth)]
    [(string=? (eyemouthhair-mouth EMH) "smile")
     (underlay/offset (draw-hair EMH)
                      0 100
                      smile)]
    [(string=? (eyemouthhair-mouth EMH) "frown")
     (underlay/offset (draw-hair EMH)
                      0 100
                      frown)]
    ))
    
;change-face: Key Event -> make-eyemouthhair
;creates new eyemouthhair given keyevent
(check-expect (change-face EMH-1 "e")
              (make-eyemouthhair "normal-eyes"
                                 (eyemouthhair-mouth EMH-1)
                                 (eyemouthhair-hair EMH-1)))
(check-expect (change-face EMH-2 "e")
              (make-eyemouthhair "winking"
                                 (eyemouthhair-mouth EMH-2)
                                 (eyemouthhair-hair EMH-2)))
(check-expect (change-face EMH-1 "m")
              (make-eyemouthhair (eyemouthhair-eye EMH-1)
                                 "smile"
                                 (eyemouthhair-hair EMH-1)))
(check-expect (change-face EMH-2 "m")
              (make-eyemouthhair (eyemouthhair-eye EMH-2)
                                 "shook-mouth" (eyemouthhair-hair EMH-2)))
(check-expect (change-face EMH-1 "h")
              (make-eyemouthhair (eyemouthhair-eye EMH-1)
                                 (eyemouthhair-mouth EMH-1) "mohawk"))
(check-expect (change-face EMH-2 "h")
              (make-eyemouthhair (eyemouthhair-eye EMH-2)
                                 (eyemouthhair-mouth EMH-2) "flowery-hair"))

(define (change-face EMH ke)
  (cond
    [(and (key=? ke "e") (string=? (eyemouthhair-eye EMH) "avatarstate-eyes"))
     (make-eyemouthhair "normal-eyes" (eyemouthhair-mouth EMH) (eyemouthhair-hair EMH))]
    [(and (key=? ke "e") (string=? (eyemouthhair-eye EMH) "normal-eyes"))
     (make-eyemouthhair "winking" (eyemouthhair-mouth EMH) (eyemouthhair-hair EMH))]
    [(and (key=? ke "e") (string=? (eyemouthhair-eye EMH) "winking"))
     (make-eyemouthhair "avatarstate-eyes" (eyemouthhair-mouth EMH) (eyemouthhair-hair EMH))]
    [(and (key=? ke "m") (string=? (eyemouthhair-mouth EMH) "shook-mouth"))
     (make-eyemouthhair (eyemouthhair-eye EMH) "smile"
                        (eyemouthhair-hair EMH))]                                      
    [(and (key=? ke "m") (string=? (eyemouthhair-mouth EMH) "smile"))
     (make-eyemouthhair (eyemouthhair-eye EMH) "frown" (eyemouthhair-hair EMH))]
    [(and (key=? ke "m") (string=? (eyemouthhair-mouth EMH) "frown"))
     (make-eyemouthhair (eyemouthhair-eye EMH) "shook-mouth" (eyemouthhair-hair EMH))]
    [(and (key=? ke "h") (string=? (eyemouthhair-hair EMH) "aang-hair"))
     (make-eyemouthhair (eyemouthhair-eye EMH) (eyemouthhair-mouth EMH) "mohawk")]
    [(and (key=? ke "h") (string=? (eyemouthhair-hair EMH) "mohawk"))
     (make-eyemouthhair (eyemouthhair-eye EMH) (eyemouthhair-mouth EMH) "flowery-hair")]
    [(and (key=? ke "h") (string=? (eyemouthhair-hair EMH) "flowery-hair"))
     (make-eyemouthhair (eyemouthhair-eye EMH) (eyemouthhair-mouth EMH) "aang-hair")]
    (else EMH)
    ))

(bit-face EMH-1)