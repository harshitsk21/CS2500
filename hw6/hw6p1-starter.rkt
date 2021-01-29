;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 6, Problem 1 ==

; Consider the following functions:


; good-job : [List-of NonNegReal] -> [List-of NonNegReal]
; adds 20% to all supplied costs

(check-expect
 (good-job '())
 '())

(check-expect
 (good-job
  (cons 10 (cons 5 '())))
 (cons 12 (cons 6 '())))

(define (good-job lon)
  (do-to-all lon cons add-thanks "list"))


; add-thanks : NonNegReal -> NonNegReal
; adds 20% to the supplied cost

(check-expect (add-thanks 10) 12)
(check-expect (add-thanks 5) 6)

(define (add-thanks cost)
  (* cost 1.2))


; total-length : [List-of String] -> Nat
; returns the total length of all strings in the list

(check-expect
 (total-length
  '())
 0)

(check-expect
 (total-length
  (cons "a" (cons "bb" '())))
 3)

(define (total-length los)
  (do-to-all los + string-length "num"))


; TODO #1: abstract good-job and total-length.
; Be sure to re-define the functions using your new
; abstraction.

(check-expect (do-to-all '() + string-length "num") 0)
(check-expect (do-to-all '() + string-length "list") '())
(check-expect (do-to-all (cons "hello" (cons "world" '()))
                         + string-length "num") 10)


; do-to-all : (X, Y) [List-of X] [X X -> Y] [X -> Y] String -> Y
; Given a list, 2 functions, and expected output type, applies
; appropriate function to every element

(define (do-to-all list f1 f2 output)
  (cond
    [(and (empty? list) (string=? output "num")) 0]
    [(and (empty? list) (string=? output "list")) '()]
    [(and (empty? list) (string=? output "image")) empty-image]
    [(cons? list)
     (f1
      (f2 (first list))
      (do-to-all (rest list) f1 f2 output))]))
  

; TODO #2: use your new function to design the function
; acronym-image, which takes in a list of strings and
; visualizes them vertically stacked, with the first
; letter bolded (to highlight the acronym). The above/align
; function will be quite useful, as will the "modern" font
; (which is fixed-width, so all letters line up nicely).
; You can assume all supplied strings will have at least
; two letters.

(define LIST-1 (cons "Intermediate" (cons "Student" (cons "Language" '()))))

; acronym-image : [List-of String] -> Image
; takes a list of strings and visualizes an acronym

(define (acronym-image los)
  (do-to-all los above-align-left bold-text "image"))

; bold-text : String -> Image
; takes a string and produces an image
; with the first letter bolded

(check-expect (bold-text "Hello") (beside (text/font (substring "Hello" 0 1)
                                                     24 "black" #f
                                                     "modern" "normal" "bold" #f)
                                          (text/font (substring "Hello" 1 (string-length "Hello"))
                                                     24 "black" #f "modern" "normal" "normal" #f)))

(define (bold-text string)
  (beside
   (text/font (substring string 0 1) 24 "black" #f "modern" "normal" "bold" #f)
   (text/font (substring string 1 (string-length string))
              24 "black" #f "modern" "normal" "normal" #f)))
   
; above-align-left
; takes two images and aligns them above aligns them left

(check-expect (above-align-left (circle 10 "solid" "red")
                                (circle 10 "solid" "blue"))
              (above/align "left" (circle 10 "solid" "red")
                           (circle 10 "solid" "blue")))

(define (above-align-left image1 image2)
  (above/align "left" image1 image2))

; (acronym-image LIST-1)




