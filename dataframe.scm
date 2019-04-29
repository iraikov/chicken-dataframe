
;; Data frame implementation for CHICKEN Scheme.  
;;
;; Copyright 2019 Ivan Raikov.
;;
;; Inspired by the various data frame implementations found in R,
;; Python, and Racket.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the Lesser GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at

;; <http://www.gnu.org/licenses/>.
;;


(module dataframe
	
        (column? column-key column-collection column-properties
         column-deserialize column-serialize
         data-frame? make-data-frame df-column-properties df-collection df-column
         df-columns df-keys df-items df-row-count df-filter-columns df-select-columns
         df-insert-column df-insert-derived df-insert-columns df-update-column df-delete-column
         df-for-each-column df-for-each-collection df-gen-columns df-gen-rows
         df-serialize df-deserialize
         apply-collections map-columns map-collections reduce-collections
         display.max-elements display.max-columns
         compare-symbol compare-int)

        (import scheme (chicken base) (chicken string) (chicken format)
                (prefix (only srfi-1 fold any) list.)
                (only srfi-69 symbol-hash)
          yasos yasos-collections rb-tree fmt fmt-table)

  (define display.max-elements (make-parameter 20))
  (define display.max-columns (make-parameter 10))
        
  (define *eof-object* (read (open-input-string "")))
  (define (eof-object) *eof-object*)

  (define (list-take* lst n)
    (let recur ((lst lst) (k n))
      (if (or (null? lst) (eq? 0 k)) '()
          (cons (car lst)
                (recur (cdr lst) (- k 1))))))

  
  (define-predicate column?)
  (define-operation (column-properties column))
  (define-operation (column-key column))
  (define-operation (column-collection column))
  (define-operation (column-deserialize column port))
  (define-operation (column-serialize column port))
  
  (define-record-type data-column
    (make-data-column key collection properties)
    data-column?
    (key        data-column-key set-data-column-key! )
    (collection data-column-collection set-data-column-collection! )
    (properties data-column-properties set-data-column-properties! )
    )
  
  (define-record-type derived-column
    (make-derived-column key properties parent collection)
    derived-column?
    (key        derived-column-key)
    (properties   derived-column-properties)
    (parent     derived-column-parent)
    (collection derived-column-collection)
    )

  
  (define-operation (column? <c>)
    (cond                     
     ((data-column? <c>) #t)
     ((derived-column? <c>) #t)
     (else #f)
    ))

  (define-operation (column-key <col>)
    (cond                     
     ((data-column? <col>) (data-column-key <col>))
     ((derived-column?   <col>) (derived-column-key <col>))
     (else 
      (error "operation not supported: column-key "))
     ))

  (define-operation (column-properties <col>)
    (cond                     
     ((data-column? <col>) (data-column-properties <col>))
     ((derived-column?   <col>) (derived-column-properties <col>))
     (else 
      (error "operation not supported: column-properties "))
     ))

  (define-operation (column-collection <col>)
    (cond                     
     ((data-column? <col>) (data-column-collection <col>))
     ((derived-column?   <col>) (derived-column-collection <col>))
     (else 
      (error "operation not supported: column-collection "))
     ))

  (define-operation (column-deserialize <col> port)
    (cond                     
     ((data-column? <col>)
      (let* ((data (read port))
             (k (car data))
             (p (cadr data))
             (x (caddr data)))
        (set-data-column-key! <col> k)
        (set-data-column-properties! <col> p)
        (set-data-column-collection! <col> x)
        ))
     (else 
      (error "operation not supported: column-deserialize"))
     ))

  (define-operation (column-serialize <col> port)
    (let ((k (column-key <col>))
          (p (column-properties <col>))
          (c (column-collection <col>)))
      (display #\( port)
      (write k port) (display #\space port)
      (write p port) (display #\space port)
      (display #\( port)
      (for-each-elt (lambda (x) (write x port) (display #\space port)) c)
      (display #\) port)
      (display #\) port)
      ))

  
  (define (make-derived-collection dproc c)
    (let ( (parent c) )
      (object
       ;; collection behaviors
       ((collection? self) #t)
       ((size self) (size parent))
       ((gen-keys self) (gen-keys parent))
       ((gen-elts self) (g-map dproc (gen-elts parent)))
       ((for-each-key self proc)
        (for-each-key parent proc))
       ((for-each-elt self proc)
        (for-each-elt parent (lambda (item) (proc (dproc item)))))
       ((elt-take self n)
        (map-elts (lambda (item) (dproc item)) (elt-take parent n) ))
       ((elt-drop self n)
        (map-elts (lambda (item) (dproc item)) (elt-drop parent n) ))
       ))
      )

  
  (define-predicate data-frame?)
  (define-operation (df-column-properties data-frame key failure-object))
  (define-operation (df-collection data-frame key failure-object))
  (define-operation (df-column data-frame key failure-object))
  (define-operation (df-columns frame))
  (define-operation (df-keys frame))
  (define-operation (df-items frame))
  (define-operation (df-row-count frame))
  (define-operation (df-filter-columns frame proc))
  (define-operation (df-select-columns frame keys))
  (define-operation (df-insert-column data-frame key collection properties))
  (define-operation (df-insert-derived data-frame parent-key key proc properties))
  (define-operation (df-insert-columns data-frame lseq))
  (define-operation (df-update-column data-frame key collection properties))
  (define-operation (df-delete-column frame key))
  (define-operation (df-for-each-column data-frame proc))
  (define-operation (df-for-each-collection data-frame proc))
  (define-operation (df-gen-collections data-frame))
  (define-operation (df-gen-columns data-frame))
  (define-operation (df-gen-rows data-frame))
  (define-operation (df-serialize data-frame port))
  (define-operation (df-deserialize data-frame port))

  (define (compare-symbol x y)
    (- (symbol-hash x) (symbol-hash y)))
  
  (define (compare-int x y)
    (- x y))
  
  (define (column-generator cols)
    (let ((keys (lseq-map column-key cols)))
      (lseq-map-generator cons keys cols)
      ))
  
  (define (row-generator cols)
    (let ((gcolls
           (lseq->list
            (lseq-map
             (compose
              (lambda (x) (if (collection? x) (gen-elts x) (gen-elts (list x))))
              column-collection)
             cols))))
      (lambda ()
        (let ((row (map (lambda (g) (g)) gcolls)))
          (if (list.any eof-object? row)
              (eof-object)
              row))
        ))
    )
      
  
  (define (make-data-frame #!key
                           (column-key-compare compare-symbol)
                           (new-column-map (rb-tree-map column-key-compare)))
    (object-with-ancestors
     ( (column-map new-column-map) )
     ((data-frame? self) #t)
     ((df-column-properties self key failure-object)
      (let ((cp ((operate-as column-map get/default) self key #f)))
        (if cp (column-properties cp) failure-object)))
     ((df-collection self key failure-object)
      (let ((cp ((operate-as column-map get/default) self key #f)))
        (if cp (column-collection cp) failure-object)))
     ((df-column self key failure-object)
      ((operate-as column-map get/default) self key failure-object))
     ((df-columns self)
      (generator->lseq ((operate-as column-map gen-elts) self)))
     ((df-keys self)
      (generator->lseq ((operate-as column-map gen-keys) self)))
     ((df-items self)
      (generator->lseq
       (g-zip ((operate-as column-map gen-keys) self)
              ((operate-as column-map gen-elts) self))))
     ((df-filter-columns self proc)
      (lseq-filter (lambda (col) (proc (column-key col) (column-properties col)))
       (generator->lseq ((operate-as column-map gen-elts) self))))
     ((df-select-columns self keys)
      (lseq-filter (lambda (col) (member (column-key col) keys))
       (generator->lseq ((operate-as column-map gen-elts) self))))
     ((df-insert-column self key collection properties)
      (let* ((m1 ((operate-as column-map put) self key (make-data-column key collection properties))))
        (make-data-frame new-column-map: m1)))
     ((df-insert-derived self parent-key key proc properties)
      (let ((cmap (list.fold
                   (lambda (parent cmap)
                     (let* ((collection (make-derived-collection proc (column-collection (cdr parent))))
                            (cmap1 (put cmap key (make-derived-column key properties parent collection))))
                       cmap1))
                   column-map
                   (list (df-column self parent-key #f))
                   )))
        (make-data-frame new-column-map: cmap)))
     ((df-insert-columns self cols)
      (let recur ((cols cols) (m column-map))
        (if (null? cols)
            (make-data-frame new-column-map: m)
            (let* ((col (lseq-first cols))
                   (m1 ((operate-as column-map put) self (column-key col) col)))
              (recur (lseq-rest cols) m1)))
        ))
     ((df-update-column self key collection properties)
      (let ((m1 ((operate-as column-map update)
                 self key (make-data-column key collection properties))))
          (make-data-frame new-column-map: m1)))
     ((df-delete-column self key)
      (make-data-frame new-column-map: ((operate-as column-map delete) self key)))
     ((df-for-each-column self proc)
      ((operate-as column-map for-each-ascending)
       self (lambda (item) (proc item))))
     ((df-for-each-collection self proc)
      ((operate-as column-map for-each-ascending)
       (lambda (item) (proc (column-collection item)))))
     ((df-gen-rows self)
      (let ((cols (df-columns self)))
        (row-generator cols)))
     ((df-gen-columns self)
      (let ((cols (df-columns self)))
        (column-generator cols)))
     ((df-row-count self)
      (let* ((cols (df-columns self))
             (num-elements (size (column-collection (car cols)))))
        num-elements))
     ((df-serialize self port)
      (let* ((cols (df-columns self))
             (keys (lseq->list (lseq-map column-key cols))))
        (write keys port)
        (df-for-each-column self (lambda (item) (column-serialize (cdr item) port)))
        ))
     ((df-deserialize self port)
      (let* ((keys (read port)))
          (let ((m1 (list.fold (lambda (key m)
                                 (let ((c (make-data-column key '() '())))
                                   (column-deserialize c port)
                                   (put m key c)))
                               column-map keys)))
            (make-data-frame new-column-map: m1))
        ))
     ((show self port)
      (let* ((cols (df-columns self))
             (keys (lseq->list (lseq-map column-key cols)))
             (num-elements (size (column-collection (car cols))))
             (num-cols (length cols))
             (show-keys (list-take* keys (display.max-columns)))
             (show-cols
              (if (> (display.max-elements) 0)
                  (list-take*
                   (lseq->list
                    (lseq-map
                     (compose (lambda (c) (elt-take c (display.max-elements)))
                              column-collection)
                     cols))
                   (display.max-columns))
                  (list-take*
                   (lseq->list
                    (lseq-map
                     column-collection
                     cols))
                   (display.max-columns)))
              ))
        
          (fmt (or port (current-output-port))
               (fmt-table show-keys show-cols))
          (if (and (> (display.max-elements) 0)
                   (> num-elements (display.max-elements)))
              (fprintf (or port (current-output-port))
                       "(~A more elements ...)~%"
                       (- num-elements (display.max-elements))))
          (if (and (> (display.max-columns) 0)
                   (> num-cols (display.max-columns)))
              (fprintf (or port (current-output-port))
                       "(~A more columns ...)~%"
                       (- num-cols (display.max-columns))))
                       
          ))
      )
     )

  (define (apply-collections proc df . keys)
    (let ((ks (if (null? keys) (lseq->list (df-keys df)) keys)))
      (let ((colls (map (lambda (k) (column-collection (cdr (df-column df k #f)))) ks)))
        (apply proc colls))))
  

  (define (apply-columns proc df . keys)
    (let ((ks (if (null? keys) (lseq->list (df-keys df)) keys)))
      (let ((cols (map (lambda (k) (df-column df k #f)) ks)))
        (apply proc cols))))
  
  
  (define (map-columns proc df #!key (keys #f))
    (let ((ks (or keys (lseq->list (df-keys df)))))
      (let recur ((res (make-data-frame)) (ks ks))
          (if (null? ks)
              res
              (let ((col (cdr (df-column df (car ks) #f))))
                (recur
                 (df-insert-column 
                  res
                  (column-key col)
                  (proc col)
                  '())
                 (cdr ks))
                ))
          ))
    )
      

  (define (map-collections proc df #!key (keys #f))
    (let ((ks (or keys (lseq->list (df-keys df)))))
      (let recur ((res (make-data-frame)) (ks ks))
          (if (null? ks)
              res
              (let ((col (cdr (df-column df (car ks) #f))))
                (recur
                 (df-insert-column 
                  res
                  (column-key col)
                  (proc (column-collection col))
                  '())
                 (cdr ks))
                ))
          ))
      )
      

  (define (reduce-collections proc df seed #!key (keys #f) )
    (let ((ks (or keys (df-keys df))))
      (let ((colls (map (lambda (k) (column-collection (cdr (df-column df k #f)))) ks)))
        (let recur ((colls colls) (ax seed))
          (if (list.any null? colls) ax
              (let ((val (apply proc ax (map lseq-first colls))))
                (recur (map lseq-rest colls) val))
              ))
        ))
    )

  
)
