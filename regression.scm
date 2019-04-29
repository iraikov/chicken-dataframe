;; Linear regression and correlation on data frames

(module dataframe-regression
	
        (
         linear-regression
         correlation-coefficient
         spearman-rank-correlation
         )

        (import scheme (chicken base) (chicken format) (chicken pretty-print) fmt
                yasos yasos-collections dataframe 
                (prefix statistics s:))

  (define (linear-regression df x y)
    (apply-collections s:linear-regression df x y))
  
  (define (correlation-coefficient df x y)
    (apply-collections s:correlation-coefficient df x y))

  (define (spearman-rank-correlation df x y)
    (apply-collections s:spearman-rank-correlation df x y))

  
  
  )
