#lang racket

; Advent Of Code 2024 - Day 3
; Part 1

; "Our computers are having issues, so I have no idea if we have any Chief Historians in stock! You're welcome to check the warehouse, though," says the mildly flustered shopkeeper at the North Pole Toboggan Rental Shop. The Historians head out to take a look.

; The shopkeeper turns to you. "Any chance you can see why our computers are having issues again?"

; The computer appears to be trying to run a program, but its memory (your puzzle input) is corrupted. All of the instructions have been jumbled up!

; It seems like the goal of the program is just to multiply some numbers. It does that with instructions like mul(X,Y), where X and Y are each 1-3 digit numbers. For instance, mul(44,46) multiplies 44 by 46 to get a result of 2024. Similarly, mul(123,4) would multiply 123 by 4.

; However, because the program's memory has been corrupted, there are also many invalid characters that should be ignored, even if they look like part of a mul instruction. Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.

; For example, consider the following section of corrupted memory:

; xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
; Only the four highlighted sections are real mul instructions. Adding up the result of each instruction produces 161 (2*4 + 5*5 + 11*8 + 8*5).

(define file-name "data.txt")

(define input (file->string file-name))

(define (numberized l)
  (map string->number l))

(define operations
  (map numberized
       (regexp-match* #px"mul\\((\\d+),(\\d+)\\)" input #:match-select cdr)))

(define (compute os)
  (foldl (λ (o s)
           (+ s (foldl * 1 o))) 0 os))

(println (format "Result: ~a" (compute operations)))

; Part 2

; As you scan through the corrupted memory, you notice that some of the conditional statements are also still intact. If you handle some of the uncorrupted conditional statements in the program, you might be able to get an even more accurate result.

; There are two new instructions you'll need to handle:

; The do() instruction enables future mul instructions.
; The don't() instruction disables future mul instructions.
; Only the most recent do() or don't() instruction applies. At the beginning of the program, mul instructions are enabled.

; For example:

; xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
; This corrupted memory is similar to the example from before, but this time the mul(5,5) and mul(11,8) instructions are disabled because there is a don't() instruction before them. The other mul instructions function normally, including the one at the end that gets re-enabled by a do() instruction.

; This time, the sum of the results is 48 (2*4 + 8*5).

; I had initially planned to use the power of regexes, in particular with look-behind, but it's impossible to use a selector with a non-fixed part (.*) and performance isn't necessarily very good.
; We'll have to do some processing by hand ;)

; (?<=do\(\).*)(mul\((\d+),(\d+)\))

(define instructions
  (cons "do()" (regexp-match* #px"mul\\((\\d+),(\\d+)\\)|(do\\(\\))|(don't\\(\\))" input)))

; Well, I had to speed things up a bit, so I ended up with an “imperative” algo with set! everywhere
; But that's also the beauty of Racket, which is not only functional but also multi-paradigm!
(define (instructions-to-do l)
  (let ([acc '()] [keep? #t])
    (for/last ([i l])
      (cond
        [(and (string-prefix? i "mul") keep?) (set! acc (cons i acc))]
        [(string=? i "don't()") (set! keep? #f)]
        [(string=? i "do()") (set! keep? #t)])
      acc)))

(define operations2
  (map (λ (s) (append* (regexp-match* #px"\\((\\d+),(\\d+)\\)" s #:match-select cdr)))
       (instructions-to-do instructions)))

(println (format "Doable result: ~a" (compute (map numberized operations2))))
