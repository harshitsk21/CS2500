;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw5p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 5, Problem 1 ==

; TODO #1: Design a data definition for a list of strings.

; A ListOfStrings (LoS) is one of:
; - '()
; - (cons String LoS)
; Interpretation: a list of strings

(define LOS-0 '())
(define LOS-1 (cons "Hello" LOS-0))
(define LOS-2 (cons "World" LOS-1))
(define LOS-3 (cons "Hello" LOS-2))
(define LOS-4
  (cons "How was your day?"
        (cons "It was good" '())))


(define (los-temp los)
  (...
   (cond
     [(empty? los) ...]
     [(cons? los) ...
      (first los) ....
      (los-temp (rest los)) ....
      ]
     )))


; TODO #2: Design the function any-longer? that determines
; if any string in a supplied list of strings is longer
; than a supplied string.

; any-longer? : ListOfStrings String -> Boolean
; determines if a string in the list is longer
; than a supplied string

(check-expect (any-longer? LOS-4 "Hello") #true)
(check-expect (any-longer? LOS-4 "How is your day going?") #false)

(define (any-longer? los supplied)
  (cond
    [(empty? los) false]
    [(cons? los)
     (if (> (string-length(first los)) (string-length supplied)) #true
         (any-longer? (rest los) supplied))]))

; TODO #3: Design the function num-occurrences that counts
; the number of times a supplied string occurs within a
; list of strings.

; num-occurrences : ListOfStrings String -> NonNegNum
; counts how many times a string occurs in a list of strings

(check-expect (num-occurrences LOS-3 "Hello") 2)
(check-expect (num-occurrences LOS-3 "World") 1)

(define (num-occurrences los s)
  (cond
    [(empty? los) 0]
    [(cons? los)
     (if (string=? (first los) s)
         (+ 1 (num-occurrences (rest los) s))
         (num-occurrences (rest los) s))]))


; TODO #4: Design the function remove-occurrences that returns
; a list of strings with all occurrences of a supplied string
; removed from a supplied list of strings.

; remove-occurrences : ListOfStrings String -> ListOfStrings
; removes all occurrences of a string from a list of stringes

(check-expect (remove-occurrences LOS-3 "Hello") (cons "World" '()))
(check-expect (remove-occurrences LOS-3 "World") (cons "Hello" (cons "Hello" '())))

(define (remove-occurrences los s)
  (cond
    [(empty? los) los]
    [(cons? los)
     (if (string=? (first los) s)
         (remove-occurrences (rest los) s)
         (cons (first los) (remove-occurrences (rest los) s)))]))