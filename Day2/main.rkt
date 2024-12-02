#lang racket

; Advent Of Code 2024 - Day 2
; Part 1

; The unusual data (your puzzle input) consists of many reports, one report per line. Each report is a list of numbers called levels that are separated by spaces. For example:

; 7 6 4 2 1
; 1 2 7 8 9
; 9 7 6 2 1
; 1 3 2 4 5
; 8 6 4 4 1
; 1 3 6 7 9
; This example data contains six reports each containing five levels.

; The engineers are trying to figure out which reports are safe. The Red-Nosed reactor safety systems can only tolerate levels that are either gradually increasing or gradually decreasing. So, a report only counts as safe if both of the following are true:

; The levels are either all increasing or all decreasing.
; Any two adjacent levels differ by at least one and at most three.
; In the example above, the reports can be found safe or unsafe by checking those rules:

; 7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
; 1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
; 9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
; 1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
; 8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
; 1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.
; So, in this example, 2 reports are safe.

(define file-name "data.txt")

(define (numberized l)
  (map string->number l))

(define (list-remove l i)
  (append (take l i) (drop l (+ i 1))))

(define (maptwo f l)
  (for/and
      ([i l]
       [j (cdr l)])
    (f i j)))

(define (all-increasing? l) (maptwo < l))

(define (all-decreasing? l) (maptwo > l))

(define (all-least-one? l) (maptwo (λ (i j) (<= 1 (abs (- i j)))) l))

(define (all-most-three? l) (maptwo (λ (i j) (>= 3 (abs (- i j)))) l))

(define reports
  ;; No function composition in racket lang :(
  (map (λ (l) (numberized (string-split l)) ) (file->lines file-name)))

(define (is-report-safe? r)
  (and
   (or (all-decreasing? r) (all-increasing? r))
   (and (all-least-one? r) (all-most-three? r))))

(define safe-reports (filter is-report-safe? reports))

(println (format "Safe reports count: ~a" (length safe-reports)))

; Part 2

; The engineers are surprised by the low number of safe reports until they realize they forgot to tell you about the Problem Dampener.

; The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single bad level in what would otherwise be a safe report. It's like the bad level never happened!

; Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.

; More of the above example's reports are now safe:

; 7 6 4 2 1: Safe without removing any level.
; 1 2 7 8 9: Unsafe regardless of which level is removed.
; 9 7 6 2 1: Unsafe regardless of which level is removed.
; 1 3 2 4 5: Safe by removing the second level, 3.
; 8 6 4 4 1: Safe by removing the third level, 4.
; 1 3 6 7 9: Safe without removing any level.
; Thanks to the Problem Dampener, 4 reports are actually safe!

; This is also a valid solution for Part 1

(define (is-diff-valid? i j)
  (let ([diff (abs (- i j))]) (and (<= 1 diff) (>= 3 diff))))

(define (get-invalid-index r)
  (let* (
         [f (car r)]
         [s (cadr r)]
         [dir (if (> f s) > <)])
    (for/first
        ([i r]
         [j (cdr r)]
         [index (in-naturals)]
         #:when (not (and (dir i j) (is-diff-valid? i j))))
      index)))

(define (is-report-dampener-safe? r)
  (let (
        [invalid-index (get-invalid-index r)])
    (or
     (not invalid-index)
     (not (get-invalid-index (list-remove r invalid-index)))
     (and
      (> invalid-index 0)
      (not (get-invalid-index (list-remove r (- invalid-index 1)))))
     (not (get-invalid-index (list-remove r (+ 1 invalid-index)))))))

(define dampener-safe-reports (filter is-report-dampener-safe? reports))

(println (format "Dampener safe reports count: ~a" (length dampener-safe-reports)))