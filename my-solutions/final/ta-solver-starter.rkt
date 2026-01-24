;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt



;  PROBLEM 1:
;
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people.
;
;  Design a data definition for Chirper, including a template that is tail recursive and avoids
;  cycles.
;
;  Then design a function called most-followers which determines which user in a Chirper Network is
;  followed by the most people.
;

(define-struct user (name verified follows))
;; interp. User is (make-user String Boolean (listof User))
;;         Represents a user of Chirper with name, verification status and list of users he follows

;; Turing is the most followed
(define C1
  (shared ((-0- (make-user "Alan Turing" #false (list)))
           (-1- (make-user "John von Neumann" #false (list -0-)))
           (-2- (make-user "Richard Stallman" #true (list -0- -1- -3- -5-)))
           (-3- (make-user "Linus Torvalds" #true (list -0- -2-)))
           (-4- (make-user "Larry Wall" #true (list -2-)))
           (-5- (make-user "John McCarthy" #true (list -0- -1-))))
    -4-))

;; template
#;
(define (fn-for-user u0)
  ;; seen is (listof User); a list of users already seen
  ;; todo is (listof User); a worklist accumulator
  (local ((define (fn-for-user u todo seen)
            (if (member u todo seen)
                (fn-for-lou todo seen)
                (fn-for-lou (append (user-follows u) todo)
                            (cons u seen))))
          (define (fn-for-lou todo seen)
            (cond
              ((empty? todo) (...))
              (else
               (fn-for-user (first todo)
                            (rest todo)
                            seen)))))
    (fn-for-user u0 empty empty)))


;; User -> User
;  Given a user network, produce the user which is followed by the most people.
;  followed by the most people.
;(define (most-followers user) user) ; stub
(check-expect (most-followers C1)                                               
              (first (user-follows (first (user-follows C1)))))                 ; Alan Turing
(check-expect (most-followers (first (user-follows (first (user-follows C1))))) ; Alan Turing
              (first (user-follows (first (user-follows C1)))))                 ; no followers

(define (most-followers user)
  ;; seen is (listof User); a list of users already seen
  ;; todo is (listof User); a worklist accumulator
  ;; rsf is (listof CountFollowers); record number of followers for the user
  (local (;; CountFollowers is (make-cf User Number)
          ;; interp. represent a user with the number of his followers
          (define-struct cf (user count))
          (define (fn-for-user user todo seen rsf)
            (if (member user seen)
                (fn-for-lou todo seen
                            (update-followers-count user rsf))
                (fn-for-lou (append (user-follows user) todo)
                            (cons user seen)
                            (cons (make-cf user 1) rsf))))
          (define (fn-for-lou todo seen rsf)
            (cond
              ((empty? todo) (get-max-followers rsf))
              (else
               (fn-for-user (first todo)
                            (rest todo)
                            seen rsf))))
          ;; User (listof CountFollowers) -> (listof CountFollowers)
          ;; Increment number of followers for a given user
          (define (update-followers-count user locf)
            (map (lambda (x)
                   (if (eq? user (cf-user x))
                       (make-cf user (add1 (cf-count x)))
                       x))
                 locf))
          ;; (listof CountFollowers) -> User
          ;; Produce first user with max number of followers
          (define (get-max-followers locf)
            (local ((define (iter max todo)
                      (cond ((empty? todo) (cf-user max))
                            (else
                             (local ((define count (cf-count (first todo)))
                                     (define max-count (cf-count max)))
                               (iter (if (> count max-count)
                                         (first todo)
                                         max)
                                     (rest todo)))))))
              (iter (first locf) (rest locf)))))
    (fn-for-user user empty empty empty)))


;  PROBLEM 2:
;
;  In UBC's version of How to Code, there are often more than 800 students taking
;  the course in any given semester, meaning there are often over 40 Teaching Assistants.
;
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write
;  a program to do it for us!
;
;  Below are some data definitions for a simplified version of a TA schedule. There are some
;  number of slots that must be filled, each represented by a natural number. Each TA is
;  available for some of these slots, and has a maximum number of shifts they can work.
;
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their
;  maximum shifts. If no such schedules exist, produce false.
;
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))

(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4))
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)

; Quiz
(define ERIKA (make-ta "Erika" 1 (list 1 3 7 9)))
(define RYAN (make-ta "Ryan" 1 (list 1 8 10)))
(define REECE (make-ta "Reece" 1 (list 5 6)))
(define GORDON (make-ta "Gordon" 2 (list 2 3 9)))
(define DAVID (make-ta "David" 2 (list 2 8 9)))
(define KATIE (make-ta "Katie" 1 (list 4 6)))
(define AASHISH (make-ta "Aashish" 2 (list 1 10)))
(define GRANT (make-ta "Grant" 2 (list 1 11)))
(define RAEANNE (make-ta "Raeanne" 2 (list 1 11 12)))
(define ALEX (make-ta "Alex" 1 (list 7)))
(define ERIN (make-ta "Erin" 1 (list 4)))
(define QUIZ-TAs (list ERIKA RYAN REECE GORDON DAVID KATIE AASHISH GRANT RAEANNE))

(check-expect (false? (schedule-tas QUIZ-TAs (rest (build-list 13 identity)))) #true)
(check-expect (false? (schedule-tas
                       (cons ALEX QUIZ-TAs)                               ; + ALEX
                       (rest (build-list 13 identity))))
              #true)
(check-expect (false? (schedule-tas
                       (cons ERIN (cons ALEX QUIZ-TAs))                               ; + ALEX and ERIN
                       (rest (build-list 13 identity))))
              #false)

;(define (schedule-tas tas slots) empty) ;stub
(define (schedule-tas tas slots)
  (local (;; Schedule -> Schedule
          ;; Given a schedule, try to find a full schedule
          (define (fn-for-s s)
            (if (full-schedule? s)
                s
                (fn-for-los (next-schedules s))))
          ;; (listof Schedule) -> (Schedule | #false)
          ;; Given list of schedules, try to find a full schedule
          (define (fn-for-los los)
            (cond
              ((empty? los) #false) ; no more schedules to try, none found
              (else
               (local ((define try (fn-for-s (first los))))
                 (if (not (false? try))
                     try ; found a schedule, return it
                     (fn-for-los (rest los))))))) ; else continue the search
          ;; Schedule -> Boolean
          ;; Produce true if the given schedule is full
          ;; Assume: it's full if it contains the same number of assignments as there are slots
          ;;         It's automatically valid, because we construct only valid schedules in this function
          (define (full-schedule? s)
            (= (length slots) (length s)))
          ;; Schedule -> (listof Schedule)
          ;; Produce a list of next valid schedules from the given schedule
          (define (next-schedules s)
            (local ((define (iter slots)
                      (cond ((empty? slots) empty)
                            (else
                             (if (member (first slots) (map assignment-slot s)) ; slot is filled
                                 (iter (rest slots)) ; find slot that is not filled
                                 (make-schedules s (first slots))))))) ; fill slot with every possible ta
              (iter slots)))
          ;; Schedule Slot -> (listof Schedule)
          ;; Produce a list of valid schedules for the given schedule and new slot
          ;; Fill slot with every possible TA
          (define (make-schedules s slot)
            (local ((define (iter tas result)
                      (cond ((empty? tas) result)
                            (else
                             (local ((define try (fill-slot s slot (first tas))))
                               (iter (rest tas)
                                     (if (not (false? try))
                                         (cons try                  ; Add new valid schedule to the list
                                               result)
                                         result)))))))
              (iter tas empty)))
          ;; Schedule Slot TA -> Schedule
          ;; Return new valid schedule for the given s, slot and ta, or #false
          (define (fill-slot s slot ta)
            (if (and
                 (< (count-shifts-for-ta s ta) (ta-max ta))
                 (member slot (ta-avail ta)))
                (cons (make-assignment ta slot) s)
                #false))
          ;; Schedule TA -> Number
          ;; Return number of shifts recorded in the given schedule for the given TA
          (define (count-shifts-for-ta s ta)
            (foldl (lambda (el res)
                     (if (eq? (assignment-ta el) ta)
                         (add1 res)
                         res))
                   0 s)))
    ;; Start with empty schedule
    (fn-for-s empty)))





















           