
(import scheme (chicken base) (chicken random) srfi-1
        yasos yasos-collections dataframe dataframe-statistics dataframe-regression)

;(show-max-columns 5)
(define tdf (make-data-frame))

(define tdf1
  (df-insert-column 
   tdf
   'base
   (list-tabulate 100 (lambda (x) (- x 10)))
   '()))

(print "tdf1: " (lseq->list (df-items tdf1)))

(show tdf1 #f)


(define tdf2
  (df-insert-derived
   tdf1
   'base
   'sum
   (lambda (v) (+ v 50.1))
   '()))

(show tdf2 #f)

;; linear series
(define tdf3
  (df-insert-derived
   tdf2
   'base 'linear
   (lambda (x)
     (let ((r (/ (- (pseudo-random-integer 1000) 500) 10.0)))
       (+ r (+ (* 5 x) 12))))
   '()
   ))

;; second degree polynomial series
 (define tdf4
   (df-insert-derived
    tdf3
    'base 'second
    (lambda (x)
      (let ((r (/ (- (pseudo-random-integer 10000) 5000) 10.0)))
        (+ r (+ (* 1.5 x x) (* -2 x) 12))))
    '()
    ))

;(show tdf4 #f)

;; third degree polynomial series
(define tdf5
  (df-insert-derived 
   tdf4 'base 'third
   (lambda (x)
     (let ((r (/ (- (pseudo-random-integer 1000000) 500000) 10.0)))
       (+ r (+ (* -2.1 x x x) (* 1.5 x x) (* -2 x) 12))))
 '()
 ))

;(show tdf5 #f)

;;  exponential series
(define tdf6
  (df-insert-derived
   tdf5 'base 'exp
   (lambda (x)
     (let ((r (/ (- (pseudo-random-integer 100) 50) 10.0)))
       (+ r (* 3.5 (exp (* 0.1 x))))))
   '()
   ))

(show tdf6 #f)

;; logarithmic series
(define tdf7
  (df-insert-derived
   tdf6 'base 'log
   (lambda (x)
     (let* ((r (/ (pseudo-random-integer 100) 10.0))
            (v (+ r 5.8 (* 7.3 (log (+ 0.001 (abs x)))))))
       v
       ))
   '()
   ))
(show tdf7 #f)

;; power series
(define tdf8
  (df-insert-derived
   tdf7
   'base 'pow
   (lambda (x)
     (let ((r (/ (pseudo-random-integer 10000) 10.0)))
       (+ r (* 5.8 (expt (abs x) 7.3)))))
   '()
   ))



(show tdf8 #f)

(describe tdf8 #f)

(define tdf9
  ;; Add a second base, which contains only positive values, logarithmic and
  ;; power law fitting can only be done on positive values.
  (df-insert-derived
   tdf2
   'base
   'abs
   (lambda (v) (abs v))
   '()))

(print (linear-regression tdf9 'abs 'sum))

(df-serialize tdf8 (current-output-port))

