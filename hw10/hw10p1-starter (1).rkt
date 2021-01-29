;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |hw10p1-starter (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; == Homework 10, Problem 1 ==

; Consider a gradebook for a class.

; In order to represent how individual assignments lead to a class
; grade, you can think that there are assignment columns (each
; with a name and total points) and calculated columns (each with
; a name, operation, and columns over which to operate).
; Operations include taking the simple average, dropping some
; number of lowest values, or weighting the columns.

; For example...
; - a total column, representing a weighted average over...
;   - a homework column (worth 30% of total), representing a simple average over...
;     - 4 assignment columns (hw1-4; each out of 20pts)
;   - a project column (out of 100pts; worth 50% of total)
;   - a quizzes column (worth 20% of total), representing a weighted average over...
;     - a pre-class quizzes column (worth 20% of quizzes; with the lowest dropped), broken into...
;       - 3 columns (pcq1-3; each out of 5pts)
;     - an in-class quizzes column (worth 80% of quizzes; with the lowest dropped), broken into...
;       - 3 columns (icq1-3; each out of 10pts)


; TODO #1: using the above description, design the data for a Gradebook.
; You should represent the gradebook description above with your examples.

(define-struct assignments [name totalpoints])

(define (assignments-temp a)
  (... (assignments-name a) ...
       (assignments-totalpoints a) ...))

(define-struct calculated [name worth operations columns])

(define (calculated-temp a)
  (... (calculated-name a) ...
       (calculated-worth a) ...
       (op-temp (calculated-operations a)) ...
       (gb-temp (calculated-columns a)) ...))

; A Operation is one of 
; - NE List of Number
; - "Simple Average"
; - Natural Number
; Intepretation : An operation that will be performed on the given grades

(define OP-WA (list 40 60))
(define OP-SA "Simple Average")
(define OP-DLV 2)

(define (op-temp op)
  (...
   (cond
     [(cons? op) ...]
     [(string? op) ...]
     [(number? op) ...])))
    


; A Gradebook is one of
; - (make-assignments string nat)
; - (make-calculated  string operation [NEList-of Gradebook])


(define HOMEWORK-1 (make-assignments "homework1" 20))

(define HOMEWORK-2 (make-assignments "homework2" 20))

(define HOMEWORK-3 (make-assignments "homework3" 20))

(define HOMEWORK-4 (make-assignments "homework4" 20))

(define HOMEWORK-5 (make-assignments "" 20))

(define HOMEWORK-COLUMN (make-calculated "homework" 30 OP-SA
                                         (list HOMEWORK-1 HOMEWORK-2 HOMEWORK-3 HOMEWORK-4)))

(define HOMEWORK-COLUMN-NO-NAME (make-calculated "" 30 OP-SA
                                                 (list HOMEWORK-1 HOMEWORK-2 HOMEWORK-3 HOMEWORK-4)))

(define HOMEWORK-COLUMN-NO-NAME1 (make-calculated "homework" 30 OP-WA
                                                  (list HOMEWORK-1 HOMEWORK-2
                                                        HOMEWORK-3 HOMEWORK-4 HOMEWORK-5)))

(define PROJECT-COLUMN (make-assignments "project" 100))

(define PCQ-1 (make-assignments "pcq1" 5))

(define PCQ-2 (make-assignments "pcq2" 5))

(define PCQ-3 (make-assignments "pcq3" 5))

(define PRE-CLASS-QUIZZES (make-calculated "preclass quizzes" 20 OP-DLV (list PCQ-1 PCQ-2 PCQ-3)))

(define ICQ-1 (make-assignments "icq1" 10))

(define ICQ-2 (make-assignments "icq2" 10))

(define ICQ-3 (make-assignments "icq3" 10))

(define IN-CLASS-QUIZZES (make-calculated "inclass quizzes" 80 OP-DLV (list ICQ-1 ICQ-2 ICQ-3)))

(define QUIZ-COLUMN (make-calculated "quiz" 20 OP-WA (list PRE-CLASS-QUIZZES IN-CLASS-QUIZZES)))

(define TOTAL-COLUMN (make-calculated "total" 100 OP-DLV
                                      (list HOMEWORK-COLUMN QUIZ-COLUMN PROJECT-COLUMN)))

                                                               

(define (gb-temp gb)
  (...
   (cond
     [(assignments? gb) ...
      (assignment-temp gb)]
     [(calculated? gb)
      (calculated-temp gb)])))


; TODO #2: design the function valid-gradebook, which makes sure...
; - the names of all columns aren't empty
; - the number of dropped grades is always smaller
;   than the number of columns in that calculated column
; - the weights in a weighted average make sense: there is
;   one for each column, they are all positive, and they
;   add up to 100%

;valid-gradebook: GradeBook -> Boolean
; Makes sure a supplied gradebook is valid

(check-expect (valid-gradebook HOMEWORK-1) #true)
(check-expect (valid-gradebook HOMEWORK-COLUMN) #true)
(check-expect (valid-gradebook HOMEWORK-COLUMN-NO-NAME) #false)
(check-expect (valid-gradebook HOMEWORK-COLUMN-NO-NAME1) #false)
(check-expect (valid-gradebook PRE-CLASS-QUIZZES) #true)
(check-expect (valid-gradebook QUIZ-COLUMN) #true)
(check-expect (valid-gradebook TOTAL-COLUMN) #true)

(define (valid-gradebook gb)
  (local [; valid-names? : Gradebook -> Boolean
          ; Make sure the names of all columns aren't empty
       
          (define (valid-names? gb)
            (cond
              [(assignments? gb)
               (not (string=? "" (assignments-name gb)))]
              [(calculated? gb)
               (not (string=? "" (calculated-name gb)))]))
  
          ; valid-op? : Operator -> Boolean
          ; Checks if operator is valid 
          (define (valid-op? op)
            (cond
              [(cons? op)
               (and
                (= (length op) (length (calculated-columns gb)))
                (= (foldr + 0 op) 100))]
              [(string? op) #true]
              [(number? op) (< op (length (calculated-columns gb)))]))]
    
    (cond
      [(assignments? gb) 
       (valid-names? gb)]
      [(calculated? gb)
       (and
        (valid-names? gb)
        (valid-op? (calculated-operations gb)))])))




  


