; Write your code here

; read-csv 

; * split-string for read-csv

; Function to split a string (with conversions)
(define (split-string s)
  (map
   list->string
   (split-a-string (string->list s))
   )
  )

; Function to split a string into a list by ","
(define (split-a-string l)
  (split-a-string-helper '() l) 
  ) 

; split-a-string recursive helper
(define (split-a-string-helper t l)
    (cond
      ((null? l) (list (reverse t)))
      ((char=? (car l) #\,); If the first character is a comma...
       (if (null? t)
           (cons '() (split-a-string-helper '() (cdr l))) ; ...but it is empty, skip it
           (cons (reverse t) (split-a-string-helper '() (cdr l))))) ; ...then add reversed token to result (reverse is needed since chars are added "backwards")
      (else
       (split-a-string-helper (cons (car l) t) (cdr l)))) ; Accumulate characters in it
    )

; * read-csv main function
(define (read-csv file headers column)
  (with-input-from-file file
    (lambda ()
      (let ((remove-headers (if headers (read-line (current-input-port)) '())))
        (let loop ((lines '()) (final-list '()))
          (let ((line (read-line (current-input-port)))
                (data (if (null? lines)
                          final-list
                          (car lines))))
            (if (eof-object? line)
                (reverse final-list)
                (if (string=? line "")
                    (loop lines final-list)
                    (let ((numeric-value
                           (string->number (list-ref (split-string line) column))))
                      (loop (cons line lines) (cons numeric-value final-list)))
                    )
                )
            )
          )
        )
      )
    )
  )

; mean
(define (mean lst)
  (if (null? lst)
      0 ; avoid divison by 0
      (let ((sum (apply + lst))
            (count (length lst)))
            (approximate (/ sum count) 4))
      )
  )

; standard dev.
(define (stddev values)
  (approximate (sqrt (/
         (apply + (map (lambda (x) (* (- x (mean values)) (- x (mean values)))) values))
         (length values))
        ) 4)
  )

; regr. for a
(define (regressiona xvalues yvalues)
  (approximate (calculate-regressiona xvalues yvalues) 4)
  )


; * this function in the real one: regressiona is the function to calulate the value approximate, but this one
;   gets used in regressionb, too.
(define (calculate-regressiona xvalues yvalues)
  (let* ((n (length xvalues))
         (xmean (mean xvalues))
         (ymean (mean yvalues))
         (result
          (let loop ((i 0) (sumxy 0) (sumx2 0))
            (if (= i n)
                (/ sumxy sumx2)
                (loop (+ i 1)
                      (+ sumxy (* (- (list-ref xvalues i) xmean) (- (list-ref yvalues i) ymean)))
                      (+ sumx2 (* (- (list-ref xvalues i) xmean) (- (list-ref xvalues i) xmean))))))))
    result))

; regr. for b
 (define (regressionb xvalues yvalues)
  (approximate (- (mean yvalues) (* (calculate-regressiona xvalues yvalues) (mean xvalues))) 4)
  )


; pcc (pearson correlation coefficient)
(define (correlation xvalues yvalues)
  (let ((n (length xvalues))
        (xmean (mean xvalues))
        (ymean (mean yvalues)))
    (let loop ((i 0)
               (sum-product 0)
               (sum-x-squared 0)
               (sum-y-squared 0))
      (if (= i n)
          (approximate (if (or (= sum-x-squared 0) (= sum-y-squared 0))
              0   ; handle division by zero
              (/ sum-product (sqrt (* sum-x-squared sum-y-squared)))
              ) 4)
          (loop
           (+ i 1)
           (+ sum-product (* (- (list-ref xvalues i) xmean) (- (list-ref yvalues i) ymean)))
           (+ sum-x-squared (* (- (list-ref xvalues i) xmean) (- (list-ref xvalues i) xmean)))
           (+ sum-y-squared (* (- (list-ref yvalues i) ymean) (- (list-ref yvalues i) ymean)))
           )
          )
      )
    )
  )

; apply regression

(define (apply-regression xvalues yvalues input)
  (map (lambda (x) (approximate (+ (* x (regressiona xvalues yvalues)) (regressionb xvalues yvalues)) 4)) input)
  )

; round a number to 4 decimals
(define (approximate number dec)
  (let* ((scale (expt 10 dec))
         (scaled (* number scale))
         (rounded (round scaled)))
    (/ rounded scale)))


; Extra Credit placeholder

;(define (apply-regression sat gpa test)
    ;0.0000000001)