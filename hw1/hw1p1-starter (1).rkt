;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 1, Problem 1 ==

; Consider the function tosh defined below.
; (For functions used by tosh that aren’t familiar,
; look them up in the DrRacket documentation.)


; tosh: string number -> number (or real?)
(define (tosh e f)
  (max
   (string-length e)
   (string->number
    (substring
     (number->string
      (floor
       (abs f)))
     0 1))))

(tosh "lebron" 50)

; TODO: replace "SIGNATURE HERE" with a signature for the function tosh
; (Since this code is nonsense, do not provide a purpose statement)

; TODO: confirm your signature by defining two constants below...
; 1. Define ACTUAL-OUTPUT to be the result of calling tosh on your choice of inputs
; 2. Define EXPECTED-OUTPUT to be the result you expect to be produced by calling
;    tosh on the inputs you chose for #1.
(define ACTUAL-OUTPUT (tosh "lebron" 50))
(define EXPECTED-OUTPUT 6)
;
ACTUAL-OUTPUT
EXPECTED-OUTPUT
; You should run your code to ensure that these two constants have the same value.
