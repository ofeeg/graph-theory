(load "is-graphical.scm")
(define (vertex value vertices)
  (cons value vertices)
  )

(define (create-vertices order)
  (if (> order 0) (cons (vertex (- order 1) '()) (create-vertices (- order 1))) '()))

;;The Graphs created by this are not guaranteed to be Connected Graphs, just Graphs that satisfy the given degree sequence.
;;Also, this has problems when the last degree in the sequences is even and the length of the sequence isn't a multiple of 3.
(define (create-graph degseq)
  (define graph (create-vertices (length degseq)))
  (define (populate-vertices graph degseq)
    (if (or (null? degseq) (null? graph)) '()
	(if (eq? (car degseq) (length (cdr (car graph)))) (populate-vertices (cdr graph) (cdr degseq))
	    (cons (map (lambda (x) (graph-connect (car graph) x))
		       (let ((a
			      (filter (let ((y degseq)) (lambda (x)
							  (and (not (vertex-connected? x (car graph)))
							       (< (length (cdr x)) (list-ref y (list-index  graph x)))))) (cdr graph))))
			 (list-head a (- (car degseq) (length (cdr (car graph)))))))
			 ;;(if (> (length a) (car degseq)) (list-head a (- (car degseq) (length (cdr (car graph))))) a)))
		  (populate-vertices (cdr graph) (cdr degseq)))
	    )))
  (if (is-graphical-ErdosGallai degseq)
      (populate-vertices graph degseq) #f)
  graph
  )

(define (vertex-connected? v1 v2)
			    (not (null? (filter (lambda (x) (eq? x (car v2))) (cdr v1)))))

(define (iter-filter list flist)
  (if (null? flist) list
      (iter-filter (filter (let ((y (car flist))) (lambda (x) (not (eq? y x)))) list) (cdr flist)))
  )

(define (graph-connect vertex . vertices)
  (if (null? vertex) #f
  (if (pair? (car (car vertices)))
	     (let ((vs (car vertices)))
	         (caar (cons (map (lambda (x) (digraph-connect x vertex)) vs)
		 (map (lambda (x) (digraph-connect vertex x)) vs)
		 )))
  ;;This didn't work for some reason??
  ;;(define (connect-func connector connectee)
    ;;(set-cdr! vertex (append (cdr connector) (iter-filter (car connectee)  connector)))
  ;;connecting a normal graph is just connecting a digraph but they connect to eachother.
	     (caar (cons (map (lambda (x) (digraph-connect x vertex)) vertices)
		   (map (lambda (x) (digraph-connect vertex x)) vertices)))))
  ;;(connect-func vertex vertices)
  )

;;digraphs have one-way connections.
(define (digraph-connect vertex . vertices)
  (set-cdr! vertex (append (cdr vertex) (iter-filter (map car vertices)  vertex)))
  )

(define (get-degseq graph)
  (map (lambda (x) (length (cdr x))) graph))

(define (subgraph-rm-v graph vertices-excluded)
  (if (null? vertices-excluded) graph
      (let ((post-vertex-removal (filter (lambda (x) (not (eq? (car x) (car vertices-excluded)))) graph)))
	( subgraph-rm-v (map (lambda (x) (filter (lambda (y) (not (eq? y (car vertices-excluded)))) x))
			     post-vertex-removal) (cdr vertices-excluded)))))


(define (make-edges vertex-pair)
  ;;This does not make valid edges. This makes a bunch of edges that should be checked for validity independently.
  (if (or (null? vertex-pair) (null? (cddr vertex-pair)))
      (cons (list (car vertex-pair) (cadr vertex-pair)) '())
      (cons (list (car vertex-pair) (cadr vertex-pair)) (make-edges (cdr vertex-pair)))))

(define (edge? graph edge)
  (if (null? (filter (lambda (x) (eq? x (cadr edge))) (select-vertex graph (car edge)))) #f #t))

(define (not-empty list) (not (null? list)))

(define (remove-dupe-edges edges)
  (if (null? edges) '()
      (let ((dupe-edge (filter (lambda (edge) (and (or (eq? (caar edges) (car edge)) (eq? (cadar edges) (car edge)))
					       (or (eq? (caar edges) (cadr edge)) (eq? (cadar edges) (cadr edge))))) edges)))
	(if (> (length dupe-edge) 1) (append (filter (lambda (edge) (eq? (cadr dupe-edge) edge)) edges) (remove-dupe-edges (cdr (filter (lambda (edge) (not (eq? (cadr dupe-edge) edge))) edges)))) (append   dupe-edge (remove-dupe-edges (cdr edges)))))))

(define (get-edges-from-graph graph)
  (remove-dupe-edges  (apply append (map (lambda (x) (map (lambda (y) (list (car x) y)) (cdr x))) graph))))

(define (delete-dupes list)
  (if (null? list) '()
      (cons (car list) (delete-dupes  (filter (lambda (x) (not (eq? x (car list)))) list)))))

(define (assign-edge-lengths graph)
  (define (input-number) 
    (define input (read))
    (if (number? input) input (input-number)))
  (map (lambda (edge) (display (format #f "Assign length to edge: ~s" edge)) (list edge (input-number) )) (get-edges-from-graph g1))
  )


