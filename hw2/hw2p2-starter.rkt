;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 2, Problem 2 ==

; In each of the parts of this problem you will design
; Boolean functions with these restrictions:
;
; - in each part you are only allowed to use if,
;   the names of the parameters, #true, and #false
;   (though you may not need all of these);
;
; - you are not allowed to use an if that takes
;   the following form (if parameter #true #false),
;   since this is the same as the value of parameter;
;
; - the tests for your functions should cover ALL possible
;   input combinations for the parameters.
;
; And don't forget (for the rest of the class!), "designing" a function
; means to produce all 4 parts of the Design Recipe for functions!

; TODO: Design the function same? that takes two Boolean parameters
;       and returns true if either both are true or both are false.

;same?: boolean boolean -> boolean
;if two inputted booleans are the same outputs true,
;or outputs false if otherwise

(check-expect (same? #true #true) #true)
(check-expect (same? #false #false) #true)
(check-expect (same? #true #false) #false)
(check-expect (same? #false #true) #false)

(define (same? b1 b2)
  (if b1 (if b2 #true b2) (if b2 b1 #true)))
   
; TODO: Design the function non-agreement? that takes two Boolean parameters
;       and returns true if at least one of them is false

;non-agreement?: boolean boolean -> boolean
;if atleast one inputted boolean is false returns true,
;otherwise returns false
(check-expect (non-agreement? #true #true) #false)
(check-expect (non-agreement? #false #false) #true)
(check-expect (non-agreement? #true #false) #true)
(check-expect (non-agreement? #false #true) #true)

(define (non-agreement? c d)
  (if c (if d #false c) #true))

; TODO: Design the function follow-directions that takes two Boolean parameters:
;       * if the first is false, it simply returns the second;
;       * if the first is true, it returns the opposite of the second
;follow-directions: boolean boolean -> boolean
; if atleast one inputted boolean is false returns true, otherwise returns false
;returns second boolean if first is true, and returns opposite of second boolean
;if first is true
(check-expect (follow-directions #true #true) #false)
(check-expect (follow-directions #true #false) #true)
(check-expect (follow-directions #false #true) #true)
(check-expect (follow-directions #false #false) #false)

(define (follow-directions e f)
  (if e (if f #false e) f))

