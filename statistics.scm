;; Descriptive statistics on data frames

(module dataframe-statistics
	
        (cmin cmax mean median mode range percentile variance standard-deviation
              coefficient-of-variation describe)

        (import scheme (chicken base) (chicken format) (chicken pretty-print) fmt
                yasos yasos-collections dataframe fmt-table
                (prefix statistics s:))

  (define (cmin df)
    (map-collections (lambda (col) (reduce* min col)) df))
  (define (cmax df)
    (map-collections (lambda (col) (reduce* max col)) df))
  (define (mean df)
    (map-collections (lambda (col) (s:mean col)) df))
  (define (median df)
    (map-collections (lambda (col) (s:median col)) df))
  (define (mode df)
    (map-collections (lambda (col) (s:mode col)) df))
  (define (range df)
    (map-collections (lambda (col) (s:range col)) df))
  (define (percentile df)
    (map-collections (lambda (col) (s:percentile col)) df))
  (define (variance df)
    (map-collections (lambda (col) (s:variance col)) df))
  (define (standard-deviation df)
    (map-collections (lambda (col) (s:standard-deviation col)) df))
  (define (coefficient-of-variation df)
    (map-collections (lambda (col) (s:coefficient-of-variation col)) df))

  (define (describe df port)

    (let ((cols (lseq->list (df-columns df)))
          (means  (df-gen-rows (mean df)))
          (mins   (df-gen-rows (cmin df)))
          (maxs   (df-gen-rows (cmax df)))
          (stds   (df-gen-rows (standard-deviation df))))

      (fprintf (or port (current-output-port)) "Data frame: ~a columns, ~a rows~%"
               (length cols)
               (df-row-count df))
      (let ((output-cols
             (cons (map column-key cols)
                   (map (lambda (g) (g))
                        (list mins maxs means stds)))))

        (fmt (or port (current-output-port))
             (fmt-table (list "Column" "Min" "Max" "Mean" "Sdev")
                        output-cols))
      
      ))
    )
  )
