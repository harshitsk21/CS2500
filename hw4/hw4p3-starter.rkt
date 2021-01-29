;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; == Homework 4, Problem 3 ==

; Consider the following data definitions:

; A Genre is one of:
; - "comedy"
; - "drama"
; - "action"
; - "education"
; Interpretation: genre for a video

(define genre-comedy "comedy")
(define genre-drama "drama")
(define genre-action "action")
(define genre-education "education")

(define (genre-temp genre)
  (... genre...))

(define-struct video [name duration hd? genre next])

; A StreamingQueue is one of:
; - #false
; - (make-video String PosInteger Boolean Genre StreamingQueue)
; Interpretation: either an empty queue
; or a video with a name, duration in minutes,
; whether it's available in HD, and it's genre.
(define SQ-false #false)
(define SQ-comedy
  (make-video "Kevin Hart Comedy Special" 48 #true genre-comedy SQ-false))
(define SQ-drama
  (make-video "Game of Thrones" 64 #false genre-drama SQ-comedy))
(define SQ-action
  (make-video "Fast & Furious Moments" 24 #true genre-action SQ-drama))
(define SQ-education
  (make-video "CS 1800 Review in under 10 min" 9 #false genre-comedy SQ-action))

(define SQ-false-1 #false)
(define SQ-comedy-1
  (make-video "Kevin Hart Comedy Special" 49 #true genre-comedy SQ-false))
(define SQ-drama-1
  (make-video "Game of Thrones" 65 #false genre-drama SQ-comedy-1))
(define SQ-action-1
  (make-video "Fast & Furious Moments" 25 #true genre-action SQ-drama-1))
(define SQ-education-1
  (make-video "CS 1800 Review in under 10 min" 10 #false genre-comedy SQ-action-1))

(define (SQ-temp SQ)
  (...
   (cond
     [(boolean? SQ) ... SQ]
     [(video? SQ) ...
      (video-name SQ) ...
      (video-duration SQ) ...
      (video-hd? SQ) ...
      (video-genre SQ) ... 
      (SQ-temp (video-next SQ))]
     )))
   

; TODO #1: complete the Design Recipe for Genre
; and StreamingQueue



; TODO #2: design the function queue-pic that produces
; an image of each title (with its duration) in the queue
; stacked vertically.

(check-expect (queue-pic SQ-false)(overlay (text "No Queue" 24 "black")
                                           (square 350 "outline" "black")))

(check-expect (queue-pic SQ-comedy) (above
                                     (overlay (above (text "Kevin Hart Comedy Special" 24 "black")
                                                     (text "48 minutes" 18 "black"))
                                              (square 350 "outline" "black"))
                                     (overlay (text "No Queue" 24 "black")
                                              (square 350 "outline" "black"))))

(check-expect (queue-pic SQ-drama) (above
                                    (overlay (above (text "Game of Thrones" 24 "black")
                                                    (text "64 minutes" 18 "black"))
                                             (square 350 "outline" "black"))
                                    (overlay (above (text "Kevin Hart Comedy Special" 24 "black")
                                                    (text "48 minutes" 18 "black"))
                                             (square 350 "outline" "black"))
                                    (overlay (text "No Queue" 24 "black")
                                             (square 350 "outline" "black"))))

(check-expect (queue-pic SQ-action) (above
                                     (overlay (above (text "Fast & Furious Moments" 24 "black")
                                                     (text "24 minutes" 18 "black"))
                                              (square 350 "outline" "black"))
                                     (overlay (above (text "Game of Thrones" 24 "black")
                                                     (text "64 minutes" 18 "black"))
                                              (square 350 "outline" "black"))
                                     (overlay (above (text "Kevin Hart Comedy Special" 24 "black")
                                                     (text "48 minutes" 18 "black"))
                                              (square 350 "outline" "black"))
                                     (overlay (text "No Queue" 24 "black")
                                              (square 350 "outline" "black"))))

(check-expect (queue-pic SQ-education) (above
                                        (overlay (above
                                                  (text "CS 1800 Review in under 10 min" 24 "black")
                                                  (text "9 minutes" 18 "black"))
                                                 (square 350 "outline" "black"))
                                        (overlay (above (text "Fast & Furious Moments" 24 "black")
                                                        (text "24 minutes" 18 "black"))
                                                 (square 350 "outline" "black"))
                                        (overlay (above (text "Game of Thrones" 24 "black")
                                                        (text "64 minutes" 18 "black"))
                                                 (square 350 "outline" "black"))
                                        (overlay (above (text "Kevin Hart Comedy Special" 24 "black")
                                                        (text "48 minutes" 18 "black"))
                                                 (square 350 "outline" "black"))
                                        (overlay (text "No Queue" 24 "black")
                                                 (square 350 "outline" "black"))))

; queue-pic: StreamingQueue -> Image
; Produces vizuale representation of the queue

(define (queue-pic SQ)
  (cond
    [(boolean? SQ) (overlay
                    (text "No Queue" 24 "black") (square 350 "outline" "black"))]
    [(video? SQ)
     (above
      (overlay (above (text (video-name SQ) 24 "black")
                      (text (string-append
                             (number->string (video-duration SQ)) " minutes") 18 "black"))
               (square 350 "outline" "black"))
      (queue-pic (video-next SQ)))
     ]
    ))
   
 

; TODO #3: design the function all-hd? that determines
; if all the videos in the queue are available in HD.

; all-hd?: StreamingQueue -> Boolean
; Determines if all the videos in the queue are HD

(check-expect (all-hd? SQ-false) "no queue")
(check-expect (all-hd? SQ-comedy) #true)
(check-expect (all-hd? SQ-drama) #false)
(check-expect (all-hd? SQ-action) #false)
(check-expect (all-hd? SQ-education) #false)

(define (all-hd? SQ)
  (if
   (boolean? SQ) "no queue"
   (cond
     [(and (video? SQ) (boolean=? (video-hd? SQ) #true))
      (if (boolean?(all-hd? (video-next SQ)))
          (boolean=? (all-hd? (video-next SQ)) #true) #true)]
     [(and (video? SQ) (boolean=? (video-hd? SQ) #false)) #false]
     )
   ))



; TODO #4: design the function only-short that takes a
; queue and returns a new queue with only those videos
; in the original that are at most 12 minutes.

(check-expect (only-short SQ-education)
              (make-video "CS 1800 Review in under 10 min" 9 #false
                          genre-comedy SQ-false))

(check-expect (only-short SQ-action) SQ-false)
                                
(check-expect (only-short SQ-drama) SQ-false)
                                
(check-expect (only-short SQ-comedy) SQ-false)
                                 
(check-expect (only-short SQ-false) SQ-false)
                                 


;only-short: StreamingQueue -> StreamingQueue
;creates queues with short vidoes only

(define (only-short SQ)
  (cond
    [(boolean? SQ) SQ-false]
    [(and (video? SQ) (< (video-duration SQ) 12))
     (make-video (video-name SQ) (video-duration SQ)
                 (video-hd? SQ) (video-genre SQ)
                 (only-short (video-next SQ)))]
    [(and (video? SQ) (> (video-duration SQ) 12)) SQ-false]
    ))
      
  


; TODO #5: design the function add-ad-time that adds 1 minute
; to the duration of every video to account for requisite ads.

(check-expect (add-ad-time SQ-education) SQ-education-1)
             
(check-expect (add-ad-time SQ-action) SQ-action-1)
                                
(check-expect (add-ad-time SQ-drama) SQ-drama-1)
                                
(check-expect (add-ad-time SQ-comedy) SQ-comedy-1)
                                 
(check-expect (add-ad-time SQ-false) SQ-false)

; add-ad-time: StreamingQueue -> StreamingQueue
; Adds Ad time to the duration of every video

(define (add-ad-time SQ)
  (cond
    [(boolean? SQ) SQ-false]
    [(video? SQ) (make-video (video-name SQ)
                             (+ (video-duration SQ) 1) (video-hd? SQ) (video-genre SQ)
                             (add-ad-time (video-next SQ)))]  
    ))


; TODO #6: design the function any-funny? that determines if any
; video in the queue is in the comedy genre.

(check-expect (all-funny? SQ-false) "no queue")
(check-expect (all-funny? SQ-comedy) #true)
(check-expect (all-funny? SQ-drama) #false)
(check-expect (all-funny? SQ-action) #false)
(check-expect (all-funny? SQ-education) #false)

; all-funny?: StreamingQueue -> Boolean
; Determines if all the videos in the queue are comedy

(define (all-funny? SQ)
  (if
   (boolean? SQ) "no queue"
   (cond
     [(and (video? SQ) (string=? (video-genre SQ) genre-comedy))
      (if (boolean?(all-funny? (video-next SQ)))
          (boolean=? (all-funny? (video-next SQ)) #true) #true)]
     [(and (video? SQ) (string=? (video-genre SQ) genre-drama)) #false]
     [(and (video? SQ) (string=? (video-genre SQ) genre-action)) #false]
     [(and (video? SQ) (string=? (video-genre SQ) genre-education)) #false]
     )))




