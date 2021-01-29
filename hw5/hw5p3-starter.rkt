;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw5p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 5, Problem 3 ==

; Over a few homework assignments, we are going to build
; up to some interesting and impactful methods of text
; analysis - that is, using a computer to gain insights
; from data expressed in natural language as text.

; For now, you are going to design the major data types
; we will use throughout the rest of the project.
; So for each TODO below, follow the design recipe for
; data given the description and examples.

; (And don't worry if at this point the ideas don't make
; a lot of sense; for now, this is great practice with
; designing data using structures, itemizations, and
; lists.)



; TODO #1: design WordCount, which is used to represent
; how many times a particular word appears in a body
; (or "corpus") of text. For example, we might want to
; represent that the word "text" appeared 2 times in the
; phrase "this text has a lot of text I think!".
; (Note: your data should not include the phrase itself,
; only the word and count.)

(define-struct wordcount [word count])

; A WordCount (WC) is a (make-wordcount String NonNegInt)
; Interpretation: The number of times a word appears in a body of text
; - Word is the word we are counting
; - Count is how many times it appears

(define WC-0 (make-wordcount "Hello" 0))
(define WC-1 (make-wordcount "World" 1))
(define WC-2 (make-wordcount "Hello" 2))

(define (wc-temp wc)
  (...
   (wordcount-word wc)...
   (wordcount-count wc)...
   ))

; TODO #2: design CorpusWordCounts, which is a list of all
; the words in a corpus along with their counts. So, for
; example, in the sentence "hello world hello", the word
; "hello" appears 2 times and the word "world" appears 1
; time. You should make use of WordCount in your definition.
; (Note: your data should not include the corpus itself,
; only the words and their associated counts.)

; A CorpusWordCounts (CWC) is one of
; - '()
; - (cons WC CWC)
; Interpretation: A list of all words in a corpus and their counts

(define CWC-0 (cons WC-0 '()))
(define CWC-1 (cons WC-1 CWC-0))
(define CWC-2 (cons WC-2 CWC-1))

(define (cwc-temp cwc)
  (...
   (cond
     [(empty? cwc)...]
     [(cons? cwc)...
      (wc-temp (first cwc))...
      (cwc-temp (rest cwc))...])))

; TODO #3: design Polarity, which is a way of describing
; a body of text as being either positive, negative, or
; neutral. For example, the phrase "What a wonderful day!"
; might be labeled as positive, while "2020 has been a rough
; year :(" might be labeled as negative.
; (Note: your data should not include the text itself,
; only the label that can take these three values.)

; A Polarity (P) is one of:
; - "positive"
; - "negative"
; - "neutral"
; Interpretation: how positive a body of text is

(define P-1 "positive")
(define P-2 "negative")
(define P-3 "neutral")

(define (p-temp p)
  (...
   (cond
     [(string=? p p-1)...]
     [(string=? p p-2)...]
     [(string=? p p-3)...])))

; TODO #4: design SentenceLabel, which is a value of -1.0, 1.0,
; or 0.0 that a reader might numerically assign a particular
; sentence. We will understand these values as corresponding to
; a Polarity value (i.e., 1.0 = positive, -1.0 = negative,
; 0.0 = neutral), but your definition just needs to represent a
; type that can only take these three numeric values.

; A SentenceLabel (SL) is one of:
; - -1.0
; - 1.0
; - 0.0
; Interpretation: a number that readers may assign to sentences

(define SL-1 -1.0)
(define SL-2 1.0)
(define SL-3 0.0)

(define (sl-temp sl)
  (...
   (cond
     [(= sl sl-1)...]
     [(= sl sl-2)...]
     [(= sl sl-3)...])))

; TODO #5: design WordPolarityData, which is a way of representing
; polarity data that we collect about a word in a collection of
; sentences, including the word, the number of times it occurred in
; the corpus, and the sum of a polarity score (a positive or negative
; real number) representing how positive/negative the word has
; appeared. For example, we might represent that the word "happy"
; has occurred 4 times in a body of text, and its polarity sum is 1.2.
; As another example, perhaps the word "sad" has occurred 3 times in
; a body of text, and its polarity sum is -3.6.
; (Note: your data should not include the text itself, only the word
; and its associated polarity data.)

(define-struct wpd [word count polarity])

; A WordPolarityData (WPD) is a (make-WordPolarityData String NonNegInt Real)
; Interpretation: a way of representing polarity data where:
; - word is the word we are representing
; - count is how many times it appears
; - polarity is the sum of a polarity score

(define WPD-1 (make-wpd "Hello" 4 1.2))
(define WPD-2 (make-wpd "World" 2 0.7))

(define (wpd-temp wpd)
  (...
   (wpd-word wpd)...
   (wpd-count wpd)...
   (wpd-polarity wpd)...
   ))

; TODO #6: design CorpusWordPolarityData which is a list of all
; the words in a corpus along with their polarity data. So continuing
; the last example, we'd want to have a list with both "happy"
; (along with 4 and 1.2) as well as "sad" (along with 3 and -3.6).
; You should make use of WordPolarityData in your definition.
; (Note: your data should not include the text itself, only the words
; and their associated polarity data.)

; A CorpusWordPolarityData (CWPD) is one of:
; - '()
; - (cons WPD CWPD)
; Interpretation: A list of WPD

(define CWPD-1 '())
(define CWPD-2 (cons WPD-1 CWPD-1))
(define CWPD-3 (cons WPD-2 CWPD-2))

(define (cwpd-temp cwpd)
  (...
   (cond
     [(empty? cwpd)...]
     [(cons? cwpd)...
      (wpd-temp (first cwpd))...
      (cwpd-temp (rest cwpd))...])))