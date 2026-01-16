;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nqueens-starter--my-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;(require spd/tags) ;; No such library in OSSU's spd-starters

;; nqueens-starter.rkt


; This project involves the design of a program to solve the n queens puzzle.
;
; This starter file explains the problem and provides a few hints you can use
; to help with the solution.
;
; The key to solving this problem is to follow the recipes! It is a challenging
; problem, but if you understand how the recipes lead to the design of a Sudoku
; solve then you can follow the recipes to get to the design for this program.
;  
;
; The n queens problem consists of finding a way to place n chess queens
; on a n by n chess board while making sure that none of the queens attack each
; other. 
;
; The BOARD consists of n^2 individual SQUARES arranged in n rows of n columns.
; The colour of the squares does not matter. Each square can either be empty
; or can contain a queen.
;
; A POSITION on the board refers to a specific square.
;
; A queen ATTACKS every square in its row, its column, and both of its diagonals.
;
; A board is VALID if none of the queens placed on it attack each other.
;
; A valid board is SOLVED if it contains n queens.
;
;
; There are many strategies for solving nqueens, but you should use the following:
;  
;  - Use a backtracking search over a generated arb-arity tree that
;    is trying to add 1 queen at a time to the board. If you find a
;    valid board with n queens produce that result.
;
;  - You should design a function that consumes a natural - N - and
;    tries to find a solution.
;    
;    
;    
; NOTE 1: You can tell whether two queens are on the same diagonal by comparing
; the slope of the line between them. If one queen is at row and column (r1, c1)
; and another queen is at row and column (r2, c2) then the slope of the line
; between them is: (/ (- r2 r1) (- c2 c1)).  If that slope is 1 or -1 then the
; queens are on the same diagonal.

;; Constants:
;; ==========

(define Q #t)
(define E #f)

;; Data Definitions:
;; =================

;; Board is a (listof Square)
(define BD8-0 (build-list (* 8 8) (lambda (x) E)))
(define DB4-0 (build-list (* 4 4) (lambda (x) E)))

;; Position is (make-pos Natural Natural)
;; interp. (make-pos row col) means a square at row and column (zero based)
(define-struct pos (row column))
(define P0 (make-pos 0 0))

;; Square is Boolean
;; interp. #t means there's a queen, #f means empty square

;; Functions:
;; =========

;; Natural -> Board or #f
;; Produce solution (a board) for N queen problem or false if no solution found
;(define (nqueen n) #f) ; stub
(check-expect (nqueen 1) (list #t)) ; 1-square board
(check-expect (nqueen 2) #f) ; no solution for 2x2 board
(check-expect (nqueen 3) #f) ; no solution for 3x3 board

(define (nqueen n)
  (local (;; Solve for single board
          (define (solve--board board)
            (if (solved? board)
                board
                (solve--next-boards
                 (filter board-valid? (next-boards board)))))
          ;; (listof Board) -> Board or false
          (define (solve--next-boards lob)
            (cond
              ((empty? lob) #f)
              (else
               (local ((define try (solve--board (first lob))))
                 (if (not (false? try))
                     try
                     (solve--next-boards (rest lob))))))))
    (solve--board (build-list (* n n) (lambda (x) #f)))))

;; Board -> (listof Board)
;; Produce a list of next boards
;; ASSUME: board is valid
;; !!!
;(define (next-boards board) '()) ; stub
(check-expect (next-boards (list #f))
              (list (list #t)))
(check-expect (next-boards BD8-0)
              (map (lambda (x)
                     (build-list (* 8 8) (lambda (y) (= x y))))
                   (build-list (* 8 8) identity)))
(define (next-boards board)
  ;; Find position of the last queen (r, c)
  ;; Produce boards for positions (r+1, 0) etc..
  (local ((define new-queen (+ 1 (last-queen-pos board)))
          (define (put-queen pos)
            (set-square board pos #t)))
    (map (lambda (x)
           (put-queen x))
         (build-list (- (length board)
                        new-queen)
                     (lambda (x) (+ new-queen x))))))

;; Board Natural Val -> Board
;; Set a square in the board with value
;;
;;   pos   >>>                 0                  (add1 Pos)
;;   board vvv
;;      '()                   '()                 '()
;;   (cons Square Board)   (cons val              (cons (first board)
;;                               (rest board))      (set-square (rest board) (sub1 pos) val))
(define (set-square board pos val)
  (cond
    ((empty? board) '())
    ((zero? pos) (cons val (rest board)))
    (else
     (cons (first board)
           (set-square (rest board) (- pos 1) val)))))
  

;; Board -> Pos
;; Return position of the last queen  or -1
;(define (last-queen-pos board) #f) ; stub
(check-expect (last-queen-pos BD8-0) -1)
(check-expect (last-queen-pos
               (build-list (* 8 8)
                           (lambda (x) (or (= x 1)
                                           (= x 20)
                                           (= x 31)))))
              31)
(define (last-queen-pos board)
  (local ((define (helper board)
            (cond
              ((empty? board) 0)
              ((not (false? (first board))) 0)
              (else (+ 1 (helper (rest board))))))
          (define pos-from-end (helper (reverse board)))
          (define pos (- (length board) pos-from-end 1)))
    pos))


;; Board -> Boolean
;; Produce true if the board is valid
;; !!!
;(define (board-valid? board) #f) ; stub
(check-expect (board-valid? (build-list (* 2 2)
                                        (lambda (x) (= x 0))))
              #t)
(check-expect (board-valid? (build-list (* 4 4)
                                        (lambda (x) (or
                                                     (= x 0)
                                                     (= x 6)))))
              #t)
(check-expect (board-valid? (build-list (* 4 4)
                                        (lambda (x) (or
                                                     (= x 0)
                                                     (= x 6)
                                                     (= x 7)))))
              #f)
(define (board-valid? board)
  (local ((define queens (reverse (queen-positions board))))
    (or (empty? queens)
        (not (attack-existing? board (first queens) (rest queens))))))

;; Position (listof Position) -> Boolean
;; Produce true if given position attacks any other
;; !!!
(define (attack-existing? board pos others)
  (ormap (lambda (other)
           (attack? board pos other))
         others))

(define (attack? board p1 p2)
  (local ((define size (sqrt (length board)))
          (define (pos-x pos) (quotient pos size))
          (define (pos-y pos) (remainder pos size))
          (define x1 (pos-x p1))
          (define y1 (pos-y p1))
          (define x2 (pos-x p2))
          (define y2 (pos-y p2)))
    (or (= x1 x2)
        (= y1 y2)
        (= 1 (/ (- y2 y1) (- x2 x1)))
        (= -1 (/ (- y2 y1) (- x2 x1))))))
    

;; Board -> (listof Pos)
;; Produce a list of queen positions on a board
;; TODO: Used accumulator which I'm not supposed to use until Week 10?
(define (queen-positions board)
  (local ((define (queen-positions board pos)
            (cond
              ((empty? board) '())
              (else
               (if (not (false? (first board)))
                   (cons pos (queen-positions (rest board) (+ pos 1)))
                   (queen-positions (rest board) (+ pos 1)))))))
    (queen-positions board 0)))
          

;; Board -> Boolean
;; Produce true if the board is solved (contains sqrt(length(Board)) queens)
;; ASSUME: the board is valid
(check-expect (solved? (build-list (* 3 3) (lambda (x)
                                             (or (= x 0)
                                                 (= x 1)
                                                 (= x 2)))))
              #t)
                                                  
(define (solved? board)
  (= (sqrt (length board))
     (foldl (lambda (x sum)
              (+ sum (if x 1 0)))
            0
            board)))
  

;; Natural Natural -> Natural
;; Produce index of board element from row and column (and board size)
(define (row-col->idx size row col)
  (+ (* row size) col))

;; !!!
(define (render board)
  (local ((define size (sqrt (length board)))
          (define (render board img-full img-row idx)
            (cond
              ((empty? board) (above img-full img-row))
              ((= idx size) (render board
                                    (above img-full
                                           img-row)
                                    empty-image 0))
              (else
               (render (rest board) img-full
                       (beside img-row (overlay
                                        (square 20 "solid" (if (first board) "black" "gray"))
                                        (square 20 "outline" "black")))
                       (+ 1 idx))))))
    (render board empty-image empty-image 0)))





                   
    