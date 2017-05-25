#lang racket 

;;list of nums
(define flist (list 1 2 3 4 5 6 7 8 9))

(define (filter-three f1 f2 f3)
  (filter (lambda (x)
            (not (or (member x f1) (member x f2) (member x f3)))) flist))

;; gets nth item
(define (list-nth lst n count)
  (cond
    [(= count n) (first lst)]
    [else (list-nth (rest lst) n (add1 count))]))

;; x, y co-ordinates 
(define-struct pos (x y))

(define (build-2dlist width height f)
  (build-list height (lambda (y) (build-list
                                  width (lambda (x) (f x y))))))

(define (all-positions w h)
  (apply append (build-2dlist w h make-pos)))

(define layout (all-positions 9 9))

(define (list-empty-pos sudoku)
  (foldr (lambda (x y rest)
           (cond
             [(= x 0) (cons y rest)]
             [else rest])) empty (apply append sudoku) layout))

(define (neighbours sudoku)
  (define (row n sudoku)
    (cond
      [(= (length sudoku) (- 9 n)) (first sudoku)]
      [else (row n (rest sudoku))]))
  (define (col n matrix)
    (cond 
      [(empty? matrix) empty]
      [else (cons (list-nth (first matrix) n 0)
                  (col n (rest matrix)))]))
  (define (box x y)
    (define r (* 3 (quotient y 3)))
    (define c (* 3 (quotient x 3)))
    (define rows (list (row r sudoku) (row (+ r 1) sudoku) (row (+ r 2) sudoku)))
    (append (col c rows) (col (+ c 1) rows) (col (+ c 2) rows)))       
  (define potential-lists
    (sort 
     (map (lambda (po)
            (list po (filter-three (row (pos-y po) sudoku) (col (pos-x po) sudoku)
                                   (box (pos-x po) (pos-y po)))))
          (list-empty-pos sudoku))
     (lambda (pl1 pl2)
       (< (length (second pl1)) (length (second pl2))))))
  (define best-empty (first (first potential-lists))) 
  (define sudoku-list (apply append sudoku))
  (define (fill-in num sudoku-list pos-list)
    (cond
      [(empty? sudoku-list) empty]
      [(equal? (first pos-list) best-empty) (cons num (rest sudoku-list))]
      [else (cons (first sudoku-list) (fill-in num (rest sudoku-list) (rest pos-list)))]))
  (define (construct-sudoku sudoku-list)
    (foldr (lambda (x y)
             (cond
               [(zero? (length y)) (list (list x))]
               [(= 9 (length (first y))) (cons (list x) y)]
               [else (cons (cons x (first y)) (rest y))]))
           empty sudoku-list))
  
  (map (lambda (x)
         (construct-sudoku (fill-in x sudoku-list layout)))
       (second (first potential-lists))))

(define (solve-list sudoku-list)
  (cond
    [(empty? sudoku-list) false]
    [else 
     (local
       [(define solved (solve (first sudoku-list)))]
       (cond
         [(false? solved) (solve-list (rest sudoku-list))]
         [else solved]))]))

(define (solve sudoku)
  (cond
    [(empty? (list-empty-pos sudoku)) sudoku]
    [else (solve-list (neighbours sudoku))]))

(define (list-solutions-list sudoku-list)
  (cond
    [(empty? sudoku-list) empty]
    [else (append (list-solutions (first sudoku-list)) (list-solutions-list (rest sudoku-list)))]))

(define (list-solutions sudoku)
  (cond
    [(empty? (list-empty-pos sudoku)) (list sudoku)]
    [else (list-solutions-list (neighbours sudoku))]))

(define so-far (list 0))

(define (num-solutions-list sudoku-list)
  (cond
    [(empty? sudoku-list) 0]
    [else (+ (num-solutions (first sudoku-list)) (num-solutions-list (rest sudoku-list)))]))

(define (num-solutions sudoku)
  (cond
    [(empty? (list-empty-pos sudoku))
     (begin
       (set! so-far (list (add1 (first so-far))))
       (print so-far)
       1)]
    [else (num-solutions-list (neighbours sudoku))]))

(define (timer fn list-args)
  (local 
    [(define tim current-inexact-milliseconds)
     (define start (tim))
     (define ans (apply fn list-args))]
    (/ (- (tim) start) 1000)))