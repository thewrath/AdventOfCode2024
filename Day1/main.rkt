#lang racket

(define file-name "input_final.txt")

(define (pair l)
    (values (first l) (second l)))

(define-values (list-a list-b)
    (for/lists (list-a list-b)
        ([i (file->lines file-name)])
        (pair (map string->number (string-split i)))))

(define distance 
    (for/sum (
        [i (sort list-a <)]
        [j (sort list-b <)]) (abs (- i j))))

(define similarity 
    (for/sum ([x list-a])
        (* x (count (lambda (y) (= x y)) list-b))))
        
(println (format "Distance: ~a" distance))
(println (format "Similarity: ~a" similarity))