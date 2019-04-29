
(module indexed-collection
	
        (indexed? index exists? collection->indexed-collection merge)

        (import scheme (chicken base)
                (prefix (only srfi-1 fold tabulate) list.)
                rb-tree yasos yasos-collections)

  (define-record-type indexed-collection
    (make-indexed-collection index data)
    indexed-collection?
    (index indexed-collection-index)
    (data indexed-collection-data)
    )

  (define-predicate indexed?)
  (define-operation (index <collection>))
  (define-operation (exists? <collection> k))
  
  (define (collection->indexed-collection collection #!key (index #f) (hash (lambda (i v) i)) (key-compare -))
    (assert (random-access? collection))
    (let* ((n (size collection))
           (end-index (- n 1))
           (cindex (or index (generate (rb-tree-map key-compare)
                                       (lambda (i) (> i end-index))
                                       (lambda (i) (hash i (elt-ref collection i)))
                                       (lambda (i) (+ i 1))
                                       0)))
           (ic (make-indexed-collection cindex collection)))
      (object
       ;; indexed collection behaviors
       ((indexed? self) #t)
       ((index self) cindex)
       ((exists? self k) (get/default cindex k #f))
       ;; collection behaviors
       ((collection? self) #t)
       ((random-access? self) #t)
       ((empty? self) (empty? collection))
       ((size self) (size collection))
       ((elt-ref self k) (elt-ref collection (get cindex k)))
       ((elt-set! self k v) (elt-set! collection (get cindex k) v))
       ((gen-keys self) (gen-keys cindex))
       ((gen-elts self) (gen-elts collection))
       ((for-each-key self proc)
        (for-each-ascending cindex proc))
       ((for-each-elt self proc)
        (for-each-elt collection proc))
       ((elt-take self n)
        (elt-take collection n))
       ((elt-drop self n)
        (elt-drop collection n))
       ))
    )


        
  (define (merge ic1 ic2 #!key
                 (merge-fn (lambda (x y) (append (or (and (list? x) x) (list x))
                                                 (or (and (list? y) y) (list y)))))
                 (transform-fn (lambda (k) (let ((c (counter)))
                                             (count (+ 1 c))
                                             c))))
    (let* ((counter (make-parameter 0))
           (index-transform ((union-transform transform-fn merge-fn)
                             (index ic1)
                             (index ic2))))
      
      (let* ((n (list-ref index-transform 0))
             (mi (list-ref index-transform 1))
             (kt1 (list-ref index-transform 2))
             (kt2 (list-ref index-transform 3))
             (icd1 (data ic1))
             (icd2 (data ic2))
             (md (make-vector n)))

        (for-each (lambda (kv1 kv2)
                    (vector-set! md (cdr kv1) (elt-ref icd1 (car kv1)))
                    (vector-set! md (cdr kv2) (elt-ref icd2 (car kv2))))
                  kt1 kt2)

        (collection->indexed-collection md index: mi)
        ))
    )

  )
