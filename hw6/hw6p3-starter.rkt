;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 6, Problem 3 ==

; TODO #1: design the function item-counts that accepts a list of
; elements, a transformation function, and an equality function,
; and counts the distinct transformation results.

; The transformation function takes an element from the list and
; produces a result. The equality function takes two results of
; the transformation function and determines if they are the same.

; As a motivating example, consider counting words (supplied as
; strings): the transformation might convert each string to lower-case,
; the equality would then compare two strings to see if they are
; equal; and the resulting counts would be a list of pairings of
; distinct-lower-case words and how many times they appeared in the
; original list. SO... in case of Peter Piper
; (https://en.wikipedia.org/wiki/Peter_Piper), supplied as a list of
; strings without punctuation, you would learn that "peter" occurs 4
; times (as do "piper", "picked", "pickled", and "peppers"); "a"
; appears 3 times, and "if" appears 1 time (as does "where", "is",
; and "the").

; The function result should be a list, where each element is a
; pairing between a distinct transformation result and a count
; of how many times that result has occurred in the original list.
; You should design this data as a first step.

; You'll then design item-counts to consider each element of the
; supplied list in order. For each element, transform it and then
; add the result to the count. Adding is a bit tricky: first you
; check if that result has already been seen (and, if so, +1 to the
; associated counter); otherwise, add a new count pair of 1 to the end.

(define-struct ic [word count])

; An item-count (IC) is a (make-ic String NonNegInt)
; Interpretation: a word and how many times it has appeared

(define IC-1 (make-ic "Hello" 5))
(define IC-2 (make-ic "World" 3))
(define IC-3 (make-ic "A" 7))

(define (ic-temp ic)
  (...
   (item-count-word ic)...
   (item-count-count ic)...))

; A [List-of X] is one of:
; - (cons x '())
; - (cons X [List-of X])
; Interpretation: a non-empty list of elements of type X

(define LOX-1 (list "Peter" "Piper" "pickled" "peppers" "Peter" "a" "a" "a"))
(define LOX-2 (list (make-ic "piper" 1)
                    (make-ic "pickled" 1)
                    (make-ic "peppers" 1)
                    (make-ic "peter" 2)
                    (make-ic "a" 3)))


(define (lox-temp lox)
  (...
   (cond
     [(empty? lox)...]
     [(cons? lox)...
      (first lox) ...
      (lox-temp (rest lox)) ...])))


; item-counts : (X, Y) [List-of X] [X -> X] [X X -> Boolean] -> [List-of Y]
; creates a list of items and their counts after each element of the
; original list was altered by the transformation function

(check-expect (item-counts LOX-1 string-downcase string=?)
              LOX-2)

(define (item-counts lox transformation equality)
  (cond
    [(empty? lox) '()]
    [(cons? lox)
     (construct-counts (list-transform lox transformation '()) equality '())]))

; list-transform : (X) [List-of X] [X -> X] [List-of X] -> [List-of X]
; applies the transformation function to every element of the list

(check-expect (list-transform LOX-1 string-downcase '())
              (reverse (list "peter" "piper" "pickled" "peppers" "peter" "a" "a" "a")))

(define (list-transform lox transformation new)
  (cond
    [(empty? lox) new]
    [(cons? lox)
     (list-transform (rest lox)
                     transformation
                     (append (list (transformation (first lox))) new))]))

; construct-counts : (X, Y) [List-of X] [X X -> Boolean] [List-of Y] -> [List-of Y]
; creates the item-counts list given the list of transformed elements

(check-expect (construct-counts (list-transform LOX-1 string-downcase '())
                                string=?
                                '())
              LOX-2)

(define (construct-counts lox equality new)
  (cond
    [(empty? lox) new]
    [(cons? lox)
     (construct-counts
      (rest lox) equality
      (add-item (first lox) equality new))]))

; add-item : (X, Y) X [X X -> Boolean] [List-of Y] -> [List-of Y]
; adds an items count if it's already in the list or creates
; a new element for that item if it isn't

(check-expect (add-item "hello" string=? LOX-2)
              (list (make-ic "hello" 1)
                    (make-ic "piper" 1)
                    (make-ic "pickled" 1)
                    (make-ic "peppers" 1)
                    (make-ic "peter" 2)
                    (make-ic "a" 3)))

(check-expect (add-item "peter" string=? LOX-2)
              (list (make-ic "piper" 1)
                    (make-ic "pickled" 1)
                    (make-ic "peppers" 1)
                    (make-ic "peter" 3)
                    (make-ic "a" 3)))

(define (add-item item equality new)
  (if (is-in-item-count? item equality new)
      (add-count item equality new)
      (create-item item new)))

; is-in-item-count? : (X, Y) X [X X -> Boolean] [List-of Y] -> Boolean
; determines if an item has already been seen

(check-expect (is-in-item-count? "peter" string=? LOX-2) #true)
(check-expect (is-in-item-count? "hello" string=? LOX-2) #false)

(define (is-in-item-count? item equality new)
  (cond
    [(empty? new) #false]
    [(cons? new)
     (if (equality item (ic-word (first new))) #true
         (is-in-item-count? item equality (rest new)))]))

; add-count : (X, Y) X [X X -> Boolean] [List-of Y] -> [List-of Y]
; finds the correct element adds 1 to an existing item count

(check-expect (add-count "peter" string=? LOX-2)
              (list (make-ic "piper" 1)
                    (make-ic "pickled" 1)
                    (make-ic "peppers" 1)
                    (make-ic "peter" 3)
                    (make-ic "a" 3)))

(define (add-count item equality list)
  (cond
    [(empty? list) '()]
    [(equality item (ic-word(first list)))
     (cons (make-ic item (add1(ic-count (first list))))
           (add-count item equality (rest list)))]
    [else
     (cons (first list)
           (add-count item equality (rest list)))]))

; create-item : (X, Y) X [List-of Y] -> [List-of Y]
; creates a item-count for an item that has not been seen

(check-expect (create-item "Hello" '())
              (list (make-ic "Hello" 1)))

(define (create-item item new)
  (append (list (make-ic item 1)) new))