;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 8, Problem 1 ==

; TODO #1: design the function my-email-style that facilitates personalizing
; messages based on a supplied opening word (such as Dear or Hi), punctuation
; used after the recipient's name (such as , or :), as well as a close for
; the message (such as -BW, which means best wishes, or Thanks!).
; For clarity, some tests have been supplied.

(check-expect
 ((my-email-style "Dear" "," "-BW") "Alice" "This is a test.")
 "Dear Alice, This is a test. -BW")

(check-expect
 ((my-email-style "Hi" ":" "Thanks!") "Alice" "This is a test.")
 "Hi Alice: This is a test. Thanks!")


; my-email-style: String String String -> String
; Takes in a opening word, punctuatiion and a ending word and outputs a message

(define (my-email-style greeting pu bye)
  (lambda (name body)
    (string-append greeting " " name pu " " body " " bye)))
