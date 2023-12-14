(load "utils.scm")

(define parse-number-with-leading-spaces
  (lambda (string index)
    (let [(char (string-ref string index))]
      (if (char=? char #\space)
        (parse-number-with-leading-spaces string (+ index 1))
        (string->number
          (let [
              (end-index (find-first-char string #\space index))
          ]
          (substring string index
            (if end-index
              end-index
              (string-length string)))))))))

; Parses the "number lists" defined in the problem.
;
; Assumes the string ends at the end of the last number, and formatting of all numbers is the same
(define parse-number-list
  (lambda (string index)
    (if (>= index (string-length string))
      '()
      (cons
        (parse-number-with-leading-spaces string index)
        (parse-number-list string (+ index 3))))))

; Returns the description of a card, (id (winning_numbers) (your_numbers))
(define line->card 
  (lambda (line)
    (let [
        (colon (find-first-char line #\: 0))
        (pipe (find-first-char line #\| 0))
      ]
      (let [
          (card
            (parse-number-with-leading-spaces line (- colon 3)))
          (winners
            (parse-number-list (substring line (+ colon 1) (- pipe 1)) 0))
          (yours
            (parse-number-list (substring line (+ pipe 1) (string-length line)) 0))
        ]
        (list card winners yours)))))

(define input
  (let loop [
      (file (open-input 4))]
    (if (port-eof? file)
      '()
      (cons
        (line->card (get-line file))
        (loop file)))))

(define card-count-winners
  (lambda (card)
    (let loop [
        (yours (caddr card))
      ]
      (if (null? yours)
        0
        (+
          (if (contains? (cadr card) (car yours)) 1 0)
          (loop (cdr yours)))))))

(define winners->score
  (lambda (winners)
    (if (= winners 0)
      0
      (expt 2 (- winners 1)))))

(begin
  (display "Part 1 Result: ")
  (display (let loop [
      (input input)
    ]
    (if (null? input)
      0
      (+ (winners->score (card-count-winners (car input))) (loop (cdr input))))
  ))
  (display "\n"))

; Converts a list of cards to a vector of (1 card)
(define cards->game
  (lambda (cards)
    (let [
        (card-with-count (lambda (card) (list 1 card)))
      ]
      (list->vector (map card-with-count cards)))))

(define copy-card-with-count
  (lambda (card-with-count)
    (let [(count (car card-with-count)) (card (cadr card-with-count))]
      (list (+ count 1) card))))

; Index here is the index of the first card to copy, not the card being evaluated
(define copy-cards
  (lambda (cards winners index)
    (if (and (> winners 0) (< index (vector-length cards)))
      (begin
        ; o no side effects
        (vector-set! cards index (copy-card-with-count (vector-ref cards index)))
        (copy-cards cards (- winners 1) (+ index 1))))))

; For each copy of the given card, evalueates it's winners (N), and copies the next N cards once (once per copy)
(define eval-card
  (lambda (cards index)
    (let [(winners
        (card-count-winners
          (cadr
            (vector-ref cards index))))]
      (let loop [(count (car (vector-ref cards index)))]
        (if (> count 0)
          (begin
            (copy-cards cards winners (+ index 1))
            (loop (- count 1))))))))

(define eval-game
  (lambda (game index)
    (if (< index (vector-length game))
      (begin
        (eval-card game index)
        (eval-game game (+ index 1))))))

(define real-game (cards->game input))
(eval-game real-game 0)
(display "Part 2 result: ")
(display
  (let loop [(index 0)]
    (if (>= index (vector-length real-game))
      0
      (+
        (car (vector-ref real-game index))
        (loop (+ index 1)))
      )))
(display "\n")
