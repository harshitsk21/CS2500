;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1p4-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 1, Problem 4 ==
;head: image -> image
;purpose: produces a head with a dialgoue line next to it
(define head(beside (line 10 10 "black") (circle 20 "solid" "maroon")))
;body: image-> image
;purpose: produces a body
(define body (line 0 100 "maroon"))
;body-hands: image->image
;(define body-hands(beside (line 30 40 "maroon") body (line 30 40 "maroon")))
;purpose: produce a body with a head and hands
(define headbody-hands (overlay (above head body) (line 60 -60 "maroon")))
;stick-text: image->image
;purpose produces a stickfigure saying a dialguge by adding legs and text to headbody-hands
(define stick-text
  (above (text "I'm not very vertically gifted" 20 "blue")
         (above headbody-hands(beside (line -30 40 "maroon")
                                      (line 30 40 "maroon")))))
stick-text






; Yours doesn't have to look like this - be creative!
; (Note: DFTBA = "Don't Forget To Be Awesome")
;
; Hint: when coming up with your function, it may be useful to separate
; parts of the body as defined constants, which allows you to then
; code something like (above HEAD BODY)

; You should include a signature and purpose for
; all functions you write.

; TODO: once your code works, publicly reply to the post in Piazza about this
; problem - include your name, where you are geographically this
; semester, and a picture produced by your function
; (you should choose the text - it can be a favorite quote,
; something about you, etc.; be creative, but also respectful).
