;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw9p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; == Homework 9, Problem 1 ==

; We are going to continue working towards the project, designing several common
; functions for analyzing a text based upon the occurrence of words.

; Some parts will reference work you did in prior homework assignments - you may
; use your own solutions to those (ideally with any corrections from grading
; feedback) or the sample solutions on the course website.

; Unless otherwise specified in the problem, you should make appropriate use of
; list abstractions and local/lambda.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reading from Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; TODO #1: design the abstraction read-from-file-with-invalid, which checks if a
; file exists (returning an empty list if it does not) and otherwise calls a
; supplied function to return a list of strings. Use this function to design
; read-words-from-file (where the list of strings is each word in the file)
; and read-lines-as-strings-from-file (where the list is each line in the file).
; Finally, design the function read-lines-as-nums-from-file, which produces
; a list of numbers, assuming every line in a file can be converted to a number.
; Some tests have been provided for clarity.


(define BAD-FILE "BADFILE.badext")

(define PETER-PIPER
  (list "peter" "piper" "picked" "a" "peck" "of" "pickled" "peppers"
        "a" "peck" "of" "pickled" "peppers" "peter" "piper" "picked"
        "if" "peter" "piper" "picked" "a" "peck" "of" "pickled"
        "peppers" "where" "is" "the" "peck" "of" "pickled" "peppers"
        "peter" "piper" "picked"))

(define SCORE-WORDS (list "happy" "sad" "panda" "movie" "work"))
(define SCORE-VALUES (list 2.2 -5 0.2 1 -1))


(check-expect
 (read-from-file-with-invalid BAD-FILE read-words)
 '())
(check-expect
 (read-from-file-with-invalid "words.txt" read-words) SCORE-WORDS) 

(check-expect
 (read-words-from-file "peter.txt")
 PETER-PIPER)

(check-expect
 (read-words-from-file "words.txt")
 SCORE-WORDS)


(check-expect
 (read-lines-as-strings-from-file "words.txt")
 SCORE-WORDS)

(check-expect
 (read-lines-as-strings-from-file "peter.txt")
 (list
  "peter piper picked a peck of pickled peppers"
  "a peck of pickled peppers peter piper picked"
  "if peter piper picked a peck of pickled peppers"
  "where is the peck of pickled peppers peter piper picked"))


(check-expect
 (read-lines-as-nums-from-file "scores.txt")
 SCORE-VALUES)

(check-expect
 (read-lines-as-nums-from-file "words.txt")
 (list #false #false #false #false #false))

; read-from-file-with-invalid: String -> [List-of Strings]
; given a filename, if file exists outputs a list of strings, or empty list if not

(define (read-from-file-with-invalid file f)
  (if (file-exists? file) (f file) '()))


;read-words-from-file: String -> [List-of Strings]
; Given a filename, produces a list of all words in the file

(define (read-words-from-file file)
  (read-from-file-with-invalid file read-words))

;read-lines-as-strings-from-file: String -> [List-of Strings]
; Given a filename, produces a list of all lines in the file

(define (read-lines-as-strings-from-file file)
  (read-from-file-with-invalid file read-lines))

; read-lines-as-nums-from-file: String -> [List-of Nat]
; Given a filename, converts all lines to a list of numbers

(define (read-lines-as-nums-from-file file)
  (map string->number (read-lines-as-strings-from-file file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; List Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; TODO #2: design the function first-k that returns a list of the first-k
; elements of a supplied list (or as many as are in the list, if that is fewer)
; Some tests have been provided for clarity. Do NOT use ISL list abstractions.

; Hint: think of both the list and number as complex inputs.



(check-expect (first-k '() 0) '())
(check-expect (first-k '() 3) '())
(check-expect (first-k (list 1 2 3) 3) (list 1 2 3))
(check-expect (first-k (list 1 2 3 4 5) 3) (list 1 2 3))
(check-expect (first-k (list "a" "b" "c") 4) (list "a" "b" "c"))



; first-k: [List-of Any] Nat -> [List-of Any]
; Given a list and a kth elemnt, produces a list that goes to k terms


(define (first-k lox n1)
  (cond
    [(and (empty? lox) (= n1 0)) '()]
    [(and (empty? lox) (> n1 0)) '()]
    [(and (cons? lox) (= n1 0)) '()]
    [(and (cons? lox) (> n1 0))
     (cons (first lox) 
           (first-k (rest lox) (- n1 1)))]))
    

; TODO #3: design the function distinct-elements that returns the distinct
; elements that occur in a list according to an equality predicate. (Another
; way of stating this is that this function returns a new list where all the
; duplicates of the first list have been removed.) Some tests have been provided
; for clarity.

; Hints:
; - One good approach to this problem is to consider each element in the supplied
;   list and ask whether it occurs in the remainder of the list: if not, keep it;
;   otherwise don't.
; - The above approach basically works, but work some examples and you'll see that
;   the order is a bit off; to help, consider what would happen if the list were
;   analyzed in reverse order.
; - In HW8 you designed a function that will be quite useful here for finding an
;   item in a list via an equality predicate :)


(check-expect
 (distinct-elements '() =)
 '())

(check-expect
 (distinct-elements (list 1 2) =)
 (list 1 2))

(check-expect
 (distinct-elements (list 2 1 2) =)
 (list 2 1))

(check-expect
 (distinct-elements (list "a" "b" "b" "c" "b" "a" "b") string=?)
 (list "a" "b" "c"))

; item-in-list? : (X) X [List-of X] [X X -> Boolean] -> Boolean
; determines if the supplied element is in the supplied list
; via the supplied equality predicate

(define (item-in-list? x lox eq?)
  (ormap (λ (xil) (eq? x xil)) lox))

; distinct-elements : [List-of Any] [X X -> Boolean] -> [List-of Any]
; Given a List and a equality predicate produces a list without any duplicates
  
(define (distinct-elements loa x)
  (cond
    [(empty? loa) '()]
    [(cons? loa)
     (cons
      (first loa)
      (if (item-in-list? (first loa) (rest loa) x)
          (remove-all (first loa) (distinct-elements (rest loa) x)) 
          (distinct-elements (rest loa) x)))]))
  
  
  
   
; TODO #4: design the function words-score that accepts three lists:
; the first is a list of words (todo) to be scored, whereas the second (words)
; and third (scores) are assumed to be the same length and parallel (that is,
; the first element in the scores list is the score corresponding to the first
; word in the words list). The function should return the total score of all the
; todo words; if a todo word can't be found in the words list, its score is the
; supplied "default" score. Some tests have been provided for clarity.

(check-expect (words-score
               '()
               SCORE-WORDS SCORE-VALUES 3.14)
              0)

(check-expect (words-score
               (list "cabbage")
               SCORE-WORDS SCORE-VALUES 3.14)
              3.14)

(check-expect (words-score
               (list "what" "a" "happy" "movie")
               SCORE-WORDS SCORE-VALUES 0)
              3.2)

(check-expect (words-score
               (list "why" "so" "sad" "panda")
               SCORE-WORDS SCORE-VALUES 0)
              -4.8)

(check-expect (words-score
               (list "watching" "a" "movie" "at" "work")
               SCORE-WORDS SCORE-VALUES 0)
              0)


; words-score : [List-of Strings] [List-of Strings] [List-of Nat] Nat -> Nat
; Given a list of todo words, list score words,
; and a list of score values, determines the sum score the words

(define (words-score lotd losw losv dn)
  (local [(define (f s1 lo1 lo2)
            (cond
              [(empty? lo1) dn]
              [(cons? lo1)
               (if (string=? s1 (first lo1)) (first lo2)
                   (f s1 (rest lo1) (rest lo2)))]))]

    (foldr + 0 (map (lambda (a) (f a losw losv)) lotd))))


  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Frequency Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO #5: design the function top-k-words that takes a list of words and
; returns the top-k words (by frequency of occurrance in the list) in
; alphabetical order. Some tests have been provided for clarity.

; This will take a few steps, most of which you've already done in this/other
; assignments; here is a suggested sequence, noting that local definitions
; are your friend :)

; 1. Count all the words (HW6)
; 2. Get the distinct counts (TODO #3)
; 3. Sort the distinct counts (biggest first)
; 4. Get the top-k of the sorted counts (TODO #2)
; 5. Get all words whose count is in the result of the previous step
; 6. Sort the resulting words alphabetically


(check-expect
 (top-k-words (list "baz" "bar" "baz" "foo") 1)
 (list "baz"))

(check-expect
 (top-k-words (list "baz" "bar" "baz" "foo") 2)
 (list "bar" "baz" "foo"))

; For reference (PETER-PIPER frequencies)...
; 4: peter piper picked peck of pickled peppers
; 3: a
; 1: if where is the

(check-expect
 (top-k-words PETER-PIPER 1)
 (list "of" "peck" "peppers" "peter" "picked" "pickled" "piper"))

(check-expect
 (top-k-words PETER-PIPER 2)
 (list "a" "of" "peck" "peppers" "peter" "picked" "pickled" "piper"))


; top-k-words :[List-of Strings] Nat -> [List-of Strings]
; Given a list of words returns list in alphabetical order via given frequency. 

(define (top-k-words los tk)
  (local [; the following code was copied from the sample solutions to HW6 P3
          (define-struct count [item ct])
          (define (item-counts lox xf eqf?)
            (cond
              [(empty? lox) '()]
              [(cons? lox)
               (add-to-count
                (xf (first lox))
                eqf?
                (item-counts (rest lox) xf eqf?))]))
          
          (define (add-to-count y eqf? loc)
            (cond
              [(empty? loc) (cons (make-count y 1) '())]
              [(cons? loc)
               (if (eqf? y (count-item (first loc)))
                   (cons (increment-count (first loc))
                         (rest loc))
                   (cons (first loc)
                         (add-to-count y eqf? (rest loc))))]))
          (define (increment-count c)
            (make-count (count-item c)
                        (add1 (count-ct c))))
          ;end of copied code
          (define (top-k-words1 los)
            (sort (item-counts los string-downcase string=?)
                  (lambda (b w) (>= (count-ct b) (count-ct w)))))
          (define (distinct-counts los)
            (distinct-elements (map count-ct (top-k-words1 los)) =))
          (define (topk n1)
            (first-k (distinct-counts los) n1))
          (define (filterkwords l n1 los)
            (cond
              [(empty? l) '()]
              [(cons? l)
               (if (= (length l) 1) (filter (lambda (g)
                                              (= (count-ct g) (first l))) (top-k-words1 los))
                   (append
                    (filter (lambda (g) (= (count-ct g)
                                           (first l))) (top-k-words1 los))
                    (filter (lambda (g) (= (count-ct g)
                                           (first (rest l)))) (top-k-words1 los))))]))] 
    (sort (map count-item (filterkwords (topk tk) tk los)) string<=?)))





          



   



