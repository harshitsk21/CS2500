;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 2, Problem 3 ==

; Your task is to design an interactive weekly exercise
; calendar. The program displays the current day and
; associated exercise, as allows the user to scroll forward/
; backward in the week by pressing the right/left arrow keys,
; respectively. You must do this according to the following
; sequence...


; TODO: Follow the Design Recipe for Weekday, which represents
;       the days of the week (Sunday - Saturday).
;  Weekday is one off:
; "Sunday"
; "Monday"
; "Tuesday"
; "Wednesday"
; "Thursday"
; "Friday"
; "Saturday"
;Intepretation: day of the week
(define WD-SUN "Sunday")
(define WD-MON "Monday")
(define WD-TUE "Tuesday")
(define WD-WED "Wednesday")
(define WD-THU "Thursday")
(define WD-FRI "Friday")
(define WD-SAT "Saturday")

(define (WD-TEMP weekday)
  (...(cond
        [(string=? weekday WD-SUN)...]
        [(string=? weekday WD-MON)...]
        [(string=? weekday WD-TUE)...]
        [(string=? weekday WD-WED)...]
        [(string=? weekday WD-THU)...]
        [(string=? weekday WD-FRI)...]
        [(string=? weekday WD-SAT)...]
        )))
   

; TODO: Design a function exercise that returns an exercise given
;       a day of the week. Here is an example, but you are
;       free to come up with your own (as long as the activity
;       varies throughout the week, so you don't get bored):
;       - Sunday:    Climbing
;       - Monday:    Cardio
;       - Tuesday:   Upper Body + Core
;       - Wednesday: Cardio
;       - Thursday:  Lower Body + Core
;       - Friday:    Cardio
;       - Saturday:  Rest

;exersise: weekday -> exercise
;outputs an excercise given the day of the week

(check-expect (exercise "Sunday") "Climbing")
(check-expect (exercise "Monday") "Cardio")
(check-expect (exercise "Tuesday") "Upper Body + Core")
(check-expect (exercise "Wednesday") "Cardio")
(check-expect (exercise "Thursday") "Lower Body + Core")
(check-expect (exercise "Friday") "Cardio")
(check-expect (exercise "Saturday") "NAP TIME")
  
(define (exercise weekday)
  (cond
    [(string=? weekday WD-SUN) "Climbing"]
    [(string=? weekday WD-MON) "Cardio"]
    [(string=? weekday WD-TUE) "Upper Body + Core"]
    [(string=? weekday WD-WED) "Cardio"]
    [(string=? weekday WD-THU) "Lower Body + Core"]
    [(string=? weekday WD-FRI) "Cardio"]
    [(string=? weekday WD-SAT) "NAP TIME"]
    ))

                             
; TODO: Design the functions next-weekday and prev-weekday.
;       The former returns the weekday after that which
;       was supplied (and Monday comes after Sunday); the
;       the latter returns the weekday before that which
;       was supplied (and Sunday comes before Monday).

;next-weekday: weekday-> weekday
; given a weekday presents the next weekday

(check-expect (next-weekday "Sunday") WD-MON)
(check-expect (next-weekday "Monday") WD-TUE)
(check-expect (next-weekday "Tuesday") WD-WED)
(check-expect (next-weekday "Wednesday") WD-THU)
(check-expect (next-weekday "Thursday") WD-FRI)
(check-expect (next-weekday "Friday") WD-SAT)
(check-expect (next-weekday "Saturday") WD-SUN)
  

(define (next-weekday weekday)
  (cond
    [(string=? weekday WD-SUN) WD-MON]
    [(string=? weekday WD-MON) WD-TUE]
    [(string=? weekday WD-TUE) WD-WED]
    [(string=? weekday WD-WED) WD-THU]
    [(string=? weekday WD-THU) WD-FRI]
    [(string=? weekday WD-FRI) WD-SAT]
    [(string=? weekday WD-SAT) WD-SUN]
    ))
;prev-weekday: weekday-> weekday
; given a weekday presents the next weekday

(check-expect (prev-weekday "Sunday") WD-SAT)
(check-expect (prev-weekday "Monday") WD-SUN)
(check-expect (prev-weekday "Tuesday") WD-MON)
(check-expect (prev-weekday "Wednesday") WD-TUE)
(check-expect (prev-weekday "Thursday") WD-WED)
(check-expect (prev-weekday "Friday") WD-THU)
(check-expect (prev-weekday "Saturday") WD-FRI)
  

(define (prev-weekday weekday)
  (cond
    [(string=? weekday WD-SUN) WD-SAT]
    [(string=? weekday WD-MON) WD-SUN]
    [(string=? weekday WD-TUE) WD-MON]
    [(string=? weekday WD-WED) WD-TUE]
    [(string=? weekday WD-THU) WD-WED]
    [(string=? weekday WD-FRI) WD-THU]
    [(string=? weekday WD-SAT) WD-FRI]
    ))


; TODO: Finally, using these pieces, design the World program
;       exercise-calendar that displays the day and associated
;       exercise, as well as allowing the user to scroll forward/
;       backward in the week by pressing the right/left keys,
;       respectively. (Hint: in BSL, "right" and "left" are the
;       KeyEvent values you need to respond to; but what about
;       all the other keys?) You should supply to this function
;       the initial day of the week to show.




;draw-calander: weekday-> image
;draws the text of the day and the excersise on a blank rectangle

(define sun-img (overlay (above (text WD-SUN 24 "maroon")
                                (text (exercise WD-SUN) 22 "blue"))
                         (rectangle 200 200 "solid" "white")))
(define mon-img (overlay (above (text WD-MON 24 "maroon")
                                (text(exercise WD-MON) 22 "blue"))
                         (rectangle 200 200 "solid" "white")))
(define tue-img (overlay (above (text WD-TUE 24 "maroon")
                                (text (exercise WD-TUE) 22 "blue"))
                         (rectangle 200 200 "solid" "white")))
(define wed-img (overlay (above (text WD-WED 24 "maroon")
                                (text(exercise WD-WED) 22 "blue"))
                         (rectangle 200 200 "solid" "white")))
(define thu-img (overlay (above (text WD-THU 24 "maroon")
                                (text (exercise WD-THU) 22 "blue"))
                         (rectangle 200 200 "solid" "white")))
(define fri-img (overlay (above (text WD-FRI 24 "maroon")
                                (text (exercise WD-FRI) 22 "blue"))
                         (rectangle 200 200 "solid" "white")))
(define sat-img (overlay (above (text WD-SAT 24 "maroon")
                                (text (exercise WD-SAT) 22 "blue"))
                         (rectangle 200 200 "solid" "white")))
(check-expect (draw-calander "Sunday") sun-img)
(check-expect (draw-calander "Monday") mon-img)
(check-expect (draw-calander "Tuesday") tue-img)
(check-expect (draw-calander "Wednesday") wed-img)
(check-expect (draw-calander "Thursday") thu-img)
(check-expect (draw-calander "Friday") fri-img)
(check-expect (draw-calander "Saturday") sat-img)

(define (draw-calander weekday)
  (cond
    [(string=? weekday WD-SUN) sun-img]
    [(string=? weekday WD-MON) mon-img]
    [(string=? weekday WD-TUE) tue-img]
    [(string=? weekday WD-WED) wed-img]
    [(string=? weekday WD-THU) thu-img]
    [(string=? weekday WD-FRI) fri-img]
    [(string=? weekday WD-SAT) sat-img]
    ))
    
     

; (check-expect (draw-calander WD-SUN) (overlay (above WD-SUN 24 "maroon"
;                  (text (exercise weekday) 22 "blue"))
;          (rectangle 200 200 "solid" "white")))
 
 
;change-day: Image Keyevent -> Image
;changes the image of the calander on keyevent
 

(check-expect (change-day "Sunday" "right") WD-MON)
(check-expect (change-day "Monday" "right") WD-TUE)
(check-expect (change-day "Tuesday" "right") WD-WED)
(check-expect (change-day "Wednesday" "right") WD-THU)
(check-expect (change-day "Thursday" "right") WD-FRI)
(check-expect (change-day "Friday" "right") WD-SAT)
(check-expect (change-day "Saturday" "right") WD-SUN)
(check-expect (change-day "Sunday" "left") WD-SAT)
(check-expect (change-day "Monday" "left") WD-SUN)
(check-expect (change-day "Tuesday" "left") WD-MON)
(check-expect (change-day "Wednesday" "left") WD-TUE)
(check-expect (change-day "Thursday" "left") WD-WED)
(check-expect (change-day "Friday" "left") WD-THU)
(check-expect (change-day "Saturday" "left") WD-FRI)

  
(define (change-day weekday ke)
  (cond
    [(key=? ke "right") (next-weekday weekday)]
    [(key=? ke "left") (prev-weekday weekday)]
    ))
  

 
;exercise-calander: weekday keyevent -> excercise
(define (exercise-calander weekday)
  (big-bang weekday
    [to-draw draw-calander]
    [on-key change-day]
    ))

 
(exercise-calander WD-WED)
 
 
