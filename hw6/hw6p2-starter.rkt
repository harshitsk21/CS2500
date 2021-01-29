;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 6, Problem 2 ==

; TODO #1: Design the function list-superlative that accepts a
; non-empty list and a value-function (which produces a number
; for each element of the list) and returns the first element
; that results in the maximal valuation. This is essentially an
; argmax and so you aren't allowed to use this function; you
; must produce a template-based, recursive solution for credit.
;
; As guiding examples...
; - given a list of the first three positive integers (1, 2, 3)
;   and the value-function identity (which just returns whatever
;   it is supplied), the function would return 3; however, if
;   the function were - (which negates the values), the function
;   would return 1.
; - given a list of (x, y) positions and posn-y as the
;   value-function, the function should return the position with
;   the largest y-coordinate
; - given a list of strings and string-length as the value-function,
;   the function should return the longest string


; A [List-of X] is one of:
; - (cons x '())
; - (cons X [List-of X])
; Interpretation: a non-empty list of elements of type X

(define POSN-LIST (cons (make-posn 1 1)
                        (cons (make-posn 3 2)
                              (cons(make-posn 5 3)
                                   (cons (make-posn 4 6)
                                         '())))))

(define STRING-LIST (cons "One"
                          (cons "Two"
                                (cons "Three"
                                      (cons "Four"
                                            '())))))

(define (lox-temp lox)
  (...
   (cond
     [(empty? lox)...]
     [(cons? lox)...
      (first lox) ...
      (lox-temp (rest lox)) ...])))


; list-superlative : (X, Y) [List-of X] [X -> Y] -> X
; returns the max element when the elemnents of the
; list have the value-function applied to it

(check-expect (list-superlative (list 1 2 3) identity) 3)
(check-expect (list-superlative (list 1 2 3) -) 1)
(check-expect (list-superlative POSN-LIST posn-y) (make-posn 4 6))
(check-expect (list-superlative STRING-LIST string-length) "Three")


(define (list-superlative list value-function)
  (cond
    [(empty? list) '()]
    [(cons? list)
     (max-element list value-function (first list))]))


; max-element : {List-of X] [X -> X] NonNegNum -> X
; determines which element has the maximum value
; after value-function is aplpied to it

(check-expect (max-element POSN-LIST posn-y
                           (first POSN-LIST)) (make-posn 4 6))
(check-expect (max-element STRING-LIST string-length
                           (first STRING-LIST)) "Three")

(define (max-element list value-function max)
  (cond
    [(empty? list) max]
    [(cons? list)
     (if (> (value-function (first list)) (value-function max))
         (max-element (rest list) value-function (first list))
         (max-element (rest list) value-function max))]))
