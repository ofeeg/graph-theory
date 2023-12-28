(load "graph.scm")

(define (select-vertex graph vertex-value)
  (car (filter (lambda (x) (eq? (car x)  vertex-value)) graph))
  )

;;This isolates the indices that either are the vertex or a vertex connected to the vertex. Can determine walks using this.
(filter number?  (let ((graph (map (lambda (y) (filter (lambda (x) (eq? 5 x)) y)) g3))) 
		   (map (lambda (x) (if (null? x) #f (list-index graph x))) graph)))


(define (walkable? graph start dest)
  (if (eq? start dest) '() ( (map (lambda (x) (select-vertex graph x)) (cdr (select-vertex graph start)))  (map (lambda (x) (select-vertex graph x)) (cdr (select-vertex graph dest))))))

;; (map (lambda (x) (list-ref g3 x)) (filter number?  (let ((graph (map (lambda (y) (filter (lambda (x) (eq? 5 x)) y)) g3))) (map (lambda (x) (if (null? x) #f (list-index graph x))) graph))))
;;$267 = ((7 11 5) (5 7))


;;(map (lambda (x) (list-ref g1 x)) (filter number?  (let ((graph (map (lambda (y) (filter (lambda (x) (eq? 5 x)) (cdr y))) g1))) (map (lambda (x) (if (null? x) #f (list-index graph x))) graph))))
;;This returns vertices that are connected to specified vertex


;;(define (not-empty list) (not (null? list)))
;;(filter not-empty (map (lambda (x) (filter (lambda (y) (eq? 1 y)) (cdr (select-vertex g1 x)))) (cdr (car g1))))
;;This returns any vertex containing the value 1 in the graph g1.
