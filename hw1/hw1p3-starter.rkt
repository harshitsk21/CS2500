;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 1, Problem 3 ==

; TODO: Define below the function salutation, which takes a first name, a last name,
; and a greeting word, and then produces a resulting combined welcome.
;
; Be careful to correct for capitalization in all inputs: no matter how
; they are supplied, all should be output with the first letter upper-case
; and the rest lower-case. For example...
;
; (salutation "hello" "jane" "doe") should result in "Hello Jane Doe"
; (salutation "WELCOME" "bOb" "SMiTh") should result in "Welcome Bob Smith"
;
; Hint: if you are doing the same process over and over... maybe that should
; be its own function?
; 
; You should include a signature and purpose for all functions you write.
; You can assume that all inputs will be a single word of at least one
; character.

;salutation: string string string -> string
;produces a welcome message using the inputed first and last name and a greeting word.

(define (salutations greeting first-name last-name)
  (string-append
   (string-append(string-upcase(substring greeting 0 1))
                 (string-downcase (substring greeting 1 (string-length greeting))))
   " "
   (string-append (string-upcase (substring first-name 0 1))
                  (string-downcase(substring first-name 1 (string-length first-name))))
   " "
   (string-append (string-upcase (substring last-name 0 1))
                  (string-downcase(substring last-name 1 (string-length last-name))))))

(salutations "hI" "pRofEssor" "derBinsky")




