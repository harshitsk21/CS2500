;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 4, Problem 1 ==

; TODO: design the data necessary to represent a book, which can
; either be physical or electronic. All books have a title, author
; and publication year. Physical books are either paperback or
; harcover, and have some number of pages. Electronic (e-books)
; have a format (pdf, epub, txt, azw, html) and a source URL.

(define-struct ebook
  [title author publication-year format url])

(define-struct physicalbook
  [title author publication-year coverstyle pages])

; An Ebook is a (make-ebook String String
; PosReal String String)
; Interpretation: an electronic book 
; - title is the name of the ebook
; - author is the person who wrote the ebook
; - publication-year is the year the ebook was published
; - format is the type of ebook (pdf, epub, txt, azw, html)
; - url is the source url of the ebook

(define ebook-1
  (make-ebook "Harry Potter" "J.K  Rowling"
              2001 "epub" "harrypotterbook.epub"))

(define (ebook-temp eb)
  (... (ebook-title eb) ...
       (ebook-author eb) ...
       (ebook-publication-year eb) ...
       (ebook-format eb) ...
       (ebook-url eb) ...))


; A Physical-Book is a (make-physicalbook
; String String PosReal String PosReal)
; Interpretation: a physical book
; - title is the name of the ebook
; - author is the person who wrote the ebook
; - publication-year is the year the ebook was published
; - coverystyle is the type of cover the book has
; (paperback/hardcover)
; - pages is the total number of pages the book has

(define physicalbook-1
  (make-ebook "Harry Potter" "J.K  Rowling"
              2001 "hardcover" "219"))

(define (physicalbook-temp pb)
  (... (physicalbook-title pb) ...
       (physicalbook-author pb) ...
       (physicalbook-publication-year pb) ...
       (physicalbook-coverstyle pb) ...
       (physicalbook-pages pb) ...))


; A Book is one of:
; - Ebook
; - Physical-Book
; Interpretation: either an electronic book or
; or a physical book, along with all necessary
; information

(define book-1 (ebook-1))
(define book-2 (physicalbook-1))

(define (book-temp book)
  (...
   (cond
     [(ebook? book) ...
      (... (ebook-title book) ...
           (ebook-author book) ...
           (ebook-publication-year book) ...
           (ebook-format book) ...
           (ebook-url book) ...)]
     [(physicalbook? book) ...
      ... (physicalbook-title book) ...
      (physicalbook-author book) ...
      (physicalbook-publication-year book) ...
      (physicalbook-coverstyle book) ...
      (physicalbook-pages book) ...
      ])))


