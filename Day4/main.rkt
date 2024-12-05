#lang racket

(require math/matrix)
(require math/array)

; Advent Of Code 2024 - Day 4
; Part 1

; "Looks like the Chief's not here. Next!" One of The Historians pulls out a device and pushes the only button on it. After a brief flash, you recognize the interior of the Ceres monitoring station!

; As the search for the Chief continues, a small Elf who lives on the station tugs on your shirt; she'd like to know if you could help her with her word search (your puzzle input). She only has to find one word: XMAS.

; This word search allows words to be horizontal, vertical, diagonal, written backwards, or even overlapping other words. It's a little unusual, though, as you don't merely need to find one instance of XMAS - you need to find all of them. Here are a few ways XMAS might appear, where irrelevant characters have been replaced with .:


; ..X...
; .SAMX.
; .A..A.
; XMAS.S
; .X....
; The actual word search will be full of letters instead. For example:

; MMMSXXMASM
; MSAMXMSMSA
; AMXSXMAAMM
; MSAMASMSMX
; XMASAMXAMM
; XXAMMXXAMA
; SMSMSASXSS
; SAXAMASAAA
; MAMMMXMMMM
; MXMXAXMASX
; In this word search, XMAS occurs a total of 18 times; here's the same word search again, but where letters not involved in any XMAS have been replaced with .:

; ....XXMAS.
; .SAMXMS...
; ...S..A...
; ..A.A.MS.X
; XMASAMX.MM
; X.....XA.A
; S.S.S.S.SS
; .A.A.A.A.A
; ..M.M.M.MM
; .X.X.XMASX

(define WORD_SIZE 3)

(define file-name "data.txt")

(define input (file->lines file-name))

(define m (list*->matrix (map string->list input)))

; Extract all words that started with letter X (with size of 4) (in 8 directions)
(define (get-x-words m)
  (let ([rs (array-size (matrix-row m 1))]
        [cs (array-size (matrix-col m 1))])
    (map list->string (filter
                       (compose not empty?)
                       (append* (for*/list ([i (in-range 0 rs)]
                                            [j (in-range 0 cs)]
                                            #:when (char=? (matrix-ref m i j) #\X))
                                  (map (λ (d) (take-word-in-dir m i j WORD_SIZE d)) (range 8))))))))

(define (take-word-in-dir m i j s d)
  (let ([nr (array-size (matrix-row m 1))]
        [nc (array-size (matrix-col m 1))])
    (cond
      [(and (= 0 d) (>= i s)) ; up
       (map (λ (k) (matrix-ref m (- i k) j))
            (range (add1 s)))]
      [(and (= 1 d) (< j (- nc s))) ; right
       (map (λ (k) (matrix-ref m i (+ j k)))
            (range (add1 s)))]
      [(and (= 2 d) (< (+ i s) nr)) ; bottom
       (map (λ (k) (matrix-ref m (+ i k) j))
            (range (add1 s)))]
      [(and (= 3 d) (>= j s)) ; left
       (map (λ (k) (matrix-ref m i (- j k)))
            (range (add1 s)))]
      [(and (= 4 d) (>= i s) (< j (- nc s))) ; up right
       (map (λ (k) (matrix-ref m (- i k) (+ j k)))
            (range (add1 s)))]
      [(and (= 5 d) (< (+ i s) nr) (< j (- nc s))) ; down right
       (map (λ (k) (matrix-ref m (+ i k) (+ j k)))
            (range (add1 s)))]
      [(and (= 6 d) (>= i s) (>= j s)) ; up left
       (map (λ (k) (matrix-ref m (- i k) (- j k)))
            (range (add1 s)))]
      [(and (= 7 d) (< (+ i s) nr) (>= j s)) ; down left
       (map (λ (k) (matrix-ref m (+ i k) (- j k)))
            (range (add1 s)))]
      [else '()])))

(define xmas (filter (λ (s) (string=? "XMAS" s)) (get-x-words m)))

(println (format "Count XMAS: ~a" (length xmas)))