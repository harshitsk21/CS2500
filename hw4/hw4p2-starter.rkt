;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 4, Problem 2 ==

; Consider the following data definition:

(define-struct processing [estimate])
(define-struct shipped [estimate])

; A PackageStatus is one of:
; - (make-processing String)
; - (make-shipped String)
; - "on the truck"
; - #true
; Interpretation: status of a package delivery,
; either processing (i.e., not yet shipped) with
; an expected ship date, already shipped with
; an expected delivery date, on the truck for
; delivery today, or already delivered (#true)

; TODO #1: finish the data design recipe for PackageStatus

(define PS-DONE #true)
(define PS-1 "on the truck")
(define PS-2 (make-shipped "9/21/2020"))
(define PS-3 (make-processing "12/21/2024"))

(define (PS-temp ps)
  (...
   (cond
     [(string? ps) ... ps]
     [(boolean? ps) ... ps]
     [(shipped? ps) ...
      (shipped-estimate ps) ...]
     [(processing? ps) ...
      (processing-estimate ps) ...
      ]
     )))

; TODO #2: design the function package-update,
; which given a package label (e.g., "my new ipad"),
; number of items in the shipment (e.g., 1), and
; PackageStatus, and outputs a status update.

; package-update: String String PackageStatus - > String 
; Gives a status update that includeds the packagage label,
; package status and number of items

(check-expect (package-update "my airpods" "1 item" PS-1)
              "my airpods (1 item) is on the truck for delivery today")
(check-expect (package-update "my airpods" "1 item" PS-2)
              "my airpods (1 item) has shipped
               and should arrive on 9/21/2020")
(check-expect (package-update "my airpods" "1 item" PS-3)
              "my airpods (1 item) is still processing and
               should ship on 12/21/2024")
(check-expect (package-update "my airpods" "1 item" PS-DONE)
              "my airpods (1 item) has been delivered")


(define (package-update label noi ps)
  (string-append
   label " (" noi ")"
   (cond
     [(string? ps) " is on the truck for delivery today"]
     [(boolean? ps) " has been delivered"]
     [(shipped? ps)
      (string-append " has shipped and should arrive on "
                     (shipped-estimate ps))]
     [(processing? ps)
      (string-append " is still processing and should ship on "
                     (processing-estimate ps))]
     )
   ))

     
 

