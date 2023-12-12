;;; Open the input file for a given day
(define open-input
  (lambda (day)
    (let ((filename (string-append "inputs/day" (number->string day))))
      (open-file-input-port filename (file-options) (buffer-mode line) (native-transcoder)))))