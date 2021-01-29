;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw5p2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; == Homework 5, Problem 2 ==

; You are to design a reminders program, which
; allows you to keep track of all the tasks in
; your busy digital life.

; In this program, a Group is an organizational unit
; that has a title (such as "Today") and a list of tasks.
; Each Task has it's own descriptive name (such as
; "Submit Homework 5") as well as an indication of whether
; the task has been completed or not.

; When you run reminders, you supply it a group; the
; program should then show you the group's title and
; the first task (including it's description and some
; way of indicating whether it's been completed or not);
; if the task list is empty, a friendly congratulations
; might be in order ;)
;
; Pressing the "n" key on the keyboard (for "next") should
; allow you to scroll through the tasks in order - it should
; cycle such that after seeing the last task, you come back to
; the first. When viewing a task, pressing space bar toggles
; whether the task is complete or not. When the program ends,
; it should return the number of tasks in the group that are
; incomplete.

; TODO #1: Finish designing the data you will need for this program.
; It will be useful (and fun!) to have a reasonable number of
; examples.

(define-struct task [desc done?])

; A Task is a (make-task String Boolean)
; Interpretation: the reminder task and whether it is complete

(define TASK-1 (make-task "Submit H.W 5" #false))
(define TASK-2 (make-task "Study for Test" #false))
(define TASK-3 (make-task "Practice Piano" #true))
(define TASK-4 (make-task "Workout" #true))

(define (task-temp task)
  (... (task-desc task) ...
       (task-done? task) ...
       )
  )

; A ListOfTasks (LoT) is one of:
; - '()
; - (cons Task LoT)
; Interpretation: a list of tasks

(define LOT-EMPTY '())
(define LOT-1 (cons TASK-1 LOT-EMPTY))
(define LOT-2 (cons TASK-2 LOT-1))
(define LOT-3 (cons TASK-3 LOT-2))
(define LOT-4 (cons TASK-4 LOT-3))

(define (lot-temp lot)
  (...
   (cond
     [(empty? lot) ...]
     [(cons? lot) ...
      (task-temp (first lot)) ...
      (lot-temp (rest lot)) ...
      ]
     )
   )
  )
  

(define-struct group [title tasks])

; A Group is a (make-group String LoT)
; Interpretation: a titled task list

(define GROUP-EMPTY (make-group "Today" LOT-EMPTY))
(define GROUP-1 (make-group "Tommorow" LOT-4))

(define (group-temp group)
  (.... (group-title group) ...
        (lot-temp (group-tasks group) ...)
        )
  )
  
; TODO #2: Finish designing the World program reminders.
; You are welcome to be creative as to how the program
; visualizes the group, the task, and completion status.
; Hint: for each function we started, follow the template
; closely to determine any further helper(s).


; reminders : Group -> Nat
; Visualizes an interactive reminders list
; and returns the number of remaining incomplete
; tasks at exit

(define (reminders initial-group)
  (num-not-complete
   (big-bang initial-group
     [to-draw draw-group]
     [on-key key-group])))


; num-not-complete : Group -> Nat
; counts the number of incomplete tasks
; in the group

(check-expect (num-not-complete GROUP-EMPTY) 0)
(check-expect (num-not-complete GROUP-1) 2)

(define (num-not-complete group)
  (cond
    [(empty? (group-tasks group)) 0]
    [(cons? (group-tasks group)) 
     (if (boolean=? (task-done? (first (group-tasks group))) #false)
         (+ 1 (num-not-complete (make-group
                                 (group-title group) (rest (group-tasks group)))))
         (num-not-complete (make-group
                            (group-title group)(rest (group-tasks group)))
                           )
         )]))
        

  
; draw-group : Group -> Image
; visualizes the reminder group

(check-expect (draw-group GROUP-EMPTY) (above (text "Today" 24 "black") empty-image))
(check-expect (draw-group GROUP-1)
              (above (text "Tommorow" 24 "black")
                     (text "Workout: Complete" 24 "black")
                     (text "Practice Piano: Complete" 24 "black")
                     (text "Study for Test: Incomplete" 24 "black")
                     (text "Submit H.W 5: Incomplete" 24  "black")))
             
               
(define (draw-group group)
  (cond
    [(empty? (group-tasks group))
     (above (text (group-title group) 24 "black") (draw-tasks (group-tasks group)))]
    [(cons? (group-tasks group)) 
     (above (text (group-title group) 24 "black") (draw-tasks (group-tasks group)))]))

; draw-task : ListOfTasks -> Image
; draws all the current tasks

(check-expect (draw-tasks LOT-4) (above (text "Workout: Complete" 24 "black")
                                        (text "Practice Piano: Complete" 24 "black")
                                        (text "Study for Test: Incomplete" 24 "black")
                                        (text "Submit H.W 5: Incomplete" 24  "black")))

(define (draw-tasks lot)
  (cond
    [(empty? lot) empty-image]
    [(cons? lot)
     (if (task-done? (first lot))
         (above (text (string-append (task-desc (first lot))
                                     ": Complete") 24 "black") (draw-tasks (rest lot)))
         (above (text (string-append (task-desc (first lot))
                                     ": Incomplete") 24 "black") (draw-tasks (rest lot))))]))
    


; key-group : Group KeyEvent -> Group
; when "n" is pressed, rotate's the group's
; task list (first goes on the end);
; when " " is pressed, flips the completion
; status of the current task

(check-expect (draw-group(key-group GROUP-1 "n"))
              (above (text "Tommorow" 24 "black")
                     (text "Submit H.W 5: Incomplete" 24  "black")               
                     (text "Study for Test: Incomplete" 24 "black")
                     (text "Practice Piano: Complete" 24 "black")
                     (text "Workout: Complete" 24 "black")
                     ))

(check-expect (draw-group(key-group GROUP-1 " "))
              (above (text "Tommorow" 24 "black")
                     (text "Workout: Incomplete" 24 "black")
                     (text "Practice Piano: Complete" 24 "black")
                     (text "Study for Test: Incomplete" 24 "black")
                     (text "Submit H.W 5: Incomplete" 24  "black")))

(define (key-group group key)
  (cond
    [(key=? key "n") (make-group (group-title group) (reverse (group-tasks group)))]
    [(key=? key " ") (make-group (group-title group) (cons
                                                      (make-task
                                                       (task-desc (first (group-tasks group)))
                                                       (not
                                                        (task-done?
                                                         (first
                                                          (group-tasks group)))))
                                                      (rest (group-tasks group))))]
    [else group]))

