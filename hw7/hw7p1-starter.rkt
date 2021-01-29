;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw7p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 7, Problem 1 ==

; For each TODO below, refer to the following data definition,
; and use pre-defined list abstraction(s) when appropriate.

; Note #1: part of the credit for each problem will be the choice
; of abstraction(s), so make sure they are a good match and lead
; to an effective function design!

; Note #2: just because we now have cool abstractions doesn't mean
; you should forget about the design recipe and following templates
; (which particularly come up for abstraction helpers)!


(define-struct conf [title authors year venue page-start page-end doi])
(define-struct journal [title authors year journal volume page-start page-end doi])

; A ResearchPublication (RP) is one of:
; - (make-conf String [NEList-of String] Nat String Nat Nat String)
; - (make-journal String [NEList-of String] Nat String Nat Nat Nat String)
; Interpretation: a research publication, either...
; - a conference paper's title, author(s), year of publication, conference name,
;   page start in the proceedings, page end in the proceedings,
;   digital object identifier
; - a journal article's title, author(s), year of publication, journal name,
;   journal volume, page start in the volume, page end in the volume,
;   digital object identifier

; (Note: a DOI is just a unique identifier for publications
;        https://en.wikipedia.org/wiki/Digital_object_identifier)


(define
  RP-CONF-1
  (make-conf
   (string-append
    "An MS in CS for non-CS Majors: "
    "Moving to Increase Diversity of Thought and Demographics in CS")
   (list "Carla Brodley" "Megan Barry" "Aidan Connell" "Catherine Gill"
         "Ian Gorton" "Benjamin Hescott" "Bryan Lackaye" "Cynthia LuBien"
         "Leena Razzaq" "Amit Shesh" "Tiffani Williams" "Andrea Danyluk")
   2020
   (string-append
    "SIGCSE '20: "
    "Proceedings of the 51st ACM Technical Symposium on Computer Science Education")
   1248 1254
   "10.1145/3328778.3366802"))

(define
  RP-CONF-2
  (make-conf
   (string-append
    "The Potential of Spatial Computing to Augment Memory: "
    "Investigating Recall in Virtual Memory Palaces")
   (list "Tara O’Grady" "Caglar Yildirim")
   2019
   "International Conference on Human-Computer Interaction"
   414 422
   "10.1007/978-3-030-23528-4_56"))

(define
  RP-JOURNAL-1
  (make-journal
   "Investigating sources of PII used in Facebook's targeted advertising"
   (list "Giridhari Venkatadri" "Elena Lucherini" "Piotr Sapiezyński" "Alan Mislove")
   2019
   "Proceedings on Privacy Enhancing Technologies"
   2019
   227 244
   "10.2478/popets-2019-0013"))

(define
  RP-JOURNAL-2
  (make-journal
   (string-append
    "Scalable Methods to Integrate Task Knowledge with "
    "the Three-Weight Algorithm for Hybrid Cognitive Processing via Optimization")
   (list "Nate Derbinsky" "José Bento" "Jonathan S. Yedidia")
   2014
   "Biologically Inspired Cognitive Architectures"
   8
   107 117
   "10.1016/j.bica.2014.03.007"))

(define LRP-1
  (list RP-CONF-1
        RP-CONF-2
        RP-JOURNAL-1
        RP-JOURNAL-2))

(define LRP-2
  (list RP-CONF-1
        RP-JOURNAL-1))

(define LRP-3
  (list RP-CONF-2
        RP-CONF-1))

; TODO #1: design the function journal-only that accepts
; a list of research publications and produces a list of
; only the journal articles.

; journal-only : [List-of Reseach Publications] -> [List-of Strings]
; Produces a list with only the journal articles

(check-expect (journal-only LRP-1) (list RP-JOURNAL-1 RP-JOURNAL-2))
(check-expect (journal-only LRP-2) (list RP-JOURNAL-1))
(check-expect (journal-only LRP-3) '())


(define (journal-only lorp)
  (filter journal? lorp))
              
; TODO #2: design the function total-pages that accepts
; a list of research publications and counts the total
; number of pages. (Hint: counting pages when given start/
; end pairs is a bit tricky; consider that if a paper
; starts at page 1 and goes through page 3, it is actually
; 3 full pages long. For reference, the provided list of
; papers is 45 pages in total.)

; total-pages : [List-of Research Publications] -> Natural Number
; Counts total number of number of pages in a list of research publications

(check-expect (total-pages LRP-1) 45)
(check-expect (total-pages LRP-2) 25)
(check-expect (total-pages LRP-3) 16)

(define (total-pages lorp)
  (foldr + 0 (map get-pagenr lorp)))

; get-pagenr : Research Publication -> Natural Number
; Determines how many pages a research publication has
(check-expect (get-pagenr RP-CONF-1) 7)
(check-expect (get-pagenr RP-CONF-2) 9)
(check-expect (get-pagenr RP-JOURNAL-1) 18)
(check-expect (get-pagenr RP-JOURNAL-2) 11)

(define (get-pagenr rp)
  (cond
    [(conf? rp)
     (+ (- (conf-page-end rp) (conf-page-start rp)) 1)]
    [(journal? rp)
     (+ (- (journal-page-end rp) (journal-page-start rp)) 1)]))


; TODO #3: design the function takes-a-village? that
; accepts a list of research publications and determines
; if all publications have at least 4 authors.

; takes-a-village? : [List-of Research Publications] -> Boolean
; Determines if a publication has more than 3 authors
(check-expect (takes-a-village? LRP-1) #false)
(check-expect (takes-a-village? LRP-2) #true)
(check-expect (takes-a-village? LRP-3) #false)

(define (takes-a-village? lorp)
  (andmap check-author lorp))

; check-author : Research Publication -> Boolean
; Checks if there is more than 3 authors
(check-expect (check-author RP-CONF-1) #true)
(check-expect (check-author RP-CONF-2) #false)
(define (check-author rp)
  (cond
    [(conf? rp)
     (>= (length (conf-authors rp))
         4)]
    [(journal? rp)
     (>= (length (journal-authors rp))
         4)]))

; TODO #4: design the function doi-urls that accepts a
; list of research publications and produces a corresponding
; list of URLS for those publications, produced by simply
; prefixing each with https://doi.org/ - try it in your
; browser to see each one of the papers in this assignment :)

; doi-urls : [List-of Research Publications] -> [List-of Strings]
; Produces list of URLS for research publication
(check-expect (doi-urls LRP-3)
              (list
               "https://doi.org/10.1007/978-3-030-23528-4_56"
               "https://doi.org/10.1145/3328778.3366802"))
(check-expect (doi-urls LRP-1) (list "https://doi.org/10.1145/3328778.3366802"
                                     "https://doi.org/10.1007/978-3-030-23528-4_56"
                                     "https://doi.org/10.2478/popets-2019-0013"
                                     "https://doi.org/10.1016/j.bica.2014.03.007"))
(define (doi-urls lorp)
  (map url-check lorp))

; url-check : Research Publication -> String
; Adds the http to url
(check-expect (url-check RP-CONF-1) "https://doi.org/10.1145/3328778.3366802")
(check-expect (url-check RP-JOURNAL-1) "https://doi.org/10.2478/popets-2019-0013")


(define (url-check rp)
  (cond
    [(conf? rp)
     (string-append "https://doi.org/" (conf-doi rp))]
    [(journal? rp)
     (string-append "https://doi.org/" (journal-doi rp))]))
     
     
      
                  
; TODO #5: design the function contains-old-research? that
; accepts a list of research publications and determines if
; any of the publications are "old", which we'll define as
; having been published before 2015 - in computing research,
; that's basically a million years ;)

; contains-old-research? : [List-of Research Publications] -> Boolean
; Determines if any of publications are published before 2015
(check-expect (contains-old-research? LRP-1) #true)
(check-expect (contains-old-research? LRP-2) #false)
(check-expect (contains-old-research? LRP-3) #false)
(define (contains-old-research? lorp)
  (ormap old? lorp))

; old? : Research Publication -> Boolean
; Checks if publication year is before 2015
(check-expect (old? RP-CONF-1) #false)
(check-expect (old? RP-CONF-2) #false)
(check-expect (old? RP-JOURNAL-1) #false)
(check-expect (old? RP-JOURNAL-2) #true)


(define (old? rp)
  (cond
    [(conf? rp)
     (< (conf-year rp) 2015)]
    [(journal? rp)
     (< (journal-year rp) 2015)]))


; TODO #6: design the function title-pyramid that accepts a
; list of research publications and produces a list of the
; publication titles, ordered by increasing length.

; title-pyramid : [List-of Research Publications] -> [List-of Strings]
; Produces list of titles, ordered by increasing length
(check-expect (title-pyramid LRP-1)
              (list
               (string-append
                "Scalable Methods to Integrate Task Knowledge with the"
                " "
                "Three-Weight Algorithm for Hybrid Cognitive Processing via Optimization")
               (string-append
                "The Potential of Spatial Computing to Augment Memory:"
                " " "Investigating Recall in Virtual Memory Palaces")
               (string-append
                "An MS in CS for non-CS Majors:"
                " " "Moving to Increase Diversity of Thought and Demographics in CS")
               (string-append
                "Investigating sources of PII used in Facebook's targeted advertising")))
(check-expect (title-pyramid LRP-2)
              (list
               (string-append "An MS in CS for non-CS Majors: Moving to"
                              " " "Increase Diversity of Thought and Demographics in CS")
               (string-append "Investigating sources of PII used in"
                              " " "Facebook's targeted advertising")))


(define (title-pyramid lorp)
  (sort (map list-title lorp) string-length>?))

; string-length>? : String String -> Boolean
; Compares length of two strings
(check-expect (string-length>? "y" "ee") #false)
(check-expect (string-length>? "yyyy" "ee") #true)
(define (string-length>? s1 s2)
  (>
   (string-length s1)
   (string-length s2)))
; list-title : Research Publication -> String
; Extracts title
(check-expect (list-title RP-CONF-1)
              (string-append
               "An MS in CS for non-CS Majors: Moving to Increase"
               " Diversity of Thought and Demographics in CS"))
(check-expect (list-title RP-JOURNAL-1)
              "Investigating sources of PII used in Facebook's targeted advertising")

(define (list-title rp)
  (cond
    [(conf? rp)
     (conf-title rp)]
    [(journal? rp)
     (journal-title rp)]))
    

; TODO #7: design the function author-list that accepts a
; list of research publications and produces a list of all
; authors (including duplicates, if there are any). The list
; should be sorted alphabetically (as the names appear; so,
; for example, "Amit Shesh" comes before "Andrea Danyluk").

; author-list : [List-of Research Publications] -> [List-of Strings]
; Creates list of authors in alphabetical order
(check-expect (author-list LRP-1) (list
                                   "Aidan Connell"
                                   "Alan Mislove"
                                   "Amit Shesh"
                                   "Andrea Danyluk"
                                   "Benjamin Hescott"
                                   "Bryan Lackaye"
                                   "Caglar Yildirim"
                                   "Carla Brodley"
                                   "Catherine Gill"
                                   "Cynthia LuBien"
                                   "Elena Lucherini"
                                   "Giridhari Venkatadri"
                                   "Ian Gorton"
                                   "Jonathan S. Yedidia"
                                   "José Bento"
                                   "Leena Razzaq"
                                   "Megan Barry"
                                   "Nate Derbinsky"
                                   "Piotr Sapiezyński"
                                   "Tara O’Grady"
                                   "Tiffani Williams"))

(define (author-list lorp)
  (sort (foldr append '() (map list-authors lorp)) string<?))

; list-authors : Research Publication -> String
; Extracts author
(define (list-authors rp)
  (cond
    [(conf? rp)
     (conf-authors rp)]
    [(journal? rp)
     (journal-authors rp)]))

; TODO #8: research papers often have a page limit and it
; is important that authors remember to save space at the
; end of their paper for a bibliography (a list of all the
; prior work they refer to in the paper); you are to design
; the function citation-space to help them. This function
; accepts a number (representing how many references they
; expect they'll need) and then produces text they can
; temporarily copy into their paper as a space saver - here
; is an example if they want to save space for 2 references...

; citation-space : Natural Number -> String
; Creates spacesavers for given number of citations
(check-expect (citation-space 1) CITATION-SPACE-1)
(check-expect (citation-space 2) CITATION-SPACE-2)
(check-expect (citation-space 3) CITATION-SPACE-3)


(define (citation-space num)
  (foldr string-append ""
         (append
          (build-list (- num 1) cp)
          (list "[" (number->string num) "] "  "author information\n"
                "title information\n"
                "venue information\n"
                "year & page information"))))
; cp : Natural Number -> String
; Creates an individual spacesaver
(check-expect (cp 1)
              (string-append
               "[2] author information\ntitle information"
               "\nvenue information\nyear & page information\n"))
(check-expect (cp 2)
              (string-append
               "[3] author information\ntitle information\nvenue "
               "information\nyear & page information\n"))
(define (cp num)
  (string-append
   "[" (number->string (+ num 1)) "] "  "author information\n"
   "title information\n"
   "venue information\n"
   "year & page information\n"))


(define
  CITATION-SPACE-1
  (string-append
   "[1] author information\n"
   "title information\n"
   "venue information\n"
   "year & page information"))

         
(define
  CITATION-SPACE-2
  (string-append
   "[1] author information\n"
   "title information\n"
   "venue information\n"
   "year & page information\n"
   "[2] author information\n"
   "title information\n"
   "venue information\n"
   "year & page information"))

(define
  CITATION-SPACE-3
  (string-append
   "[1] author information\n"
   "title information\n"
   "venue information\n"
   "year & page information\n"
   "[2] author information\n"
   "title information\n"
   "venue information\n"
   "year & page information\n"
   "[3] author information\n"
   "title information\n"
   "venue information\n"
   "year & page information"))

; The [#]'s should start at 1 and increase to the number
; supplied to the function; each should have literally the
; text shown above (re: author, title, venue, and year/page
; information; and there should be a newline ("\n") between
; each line of the result (though not after the last line).

; (Note: if you'd like to see the effect of \n, just
; use the text function on the example above.)


