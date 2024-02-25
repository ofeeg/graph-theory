
(load "graph.scm")

(define (select-vertex graph vertex-value)
  (let ((vertex (filter (lambda (x) (eq? (car x)  vertex-value)) graph)))
    (if (null? vertex) '() (car vertex)))
  )

(define (walkable? graph start dest)
  (if (eq? start dest) '() ( (map (lambda (x) (select-vertex graph x)) (cdr (select-vertex graph start)))  (map (lambda (x) (select-vertex graph x)) (cdr (select-vertex graph dest))))))


(define (not-empty list) (not (null? list)))

(define (not-empty list) (not (null? list)))
;;To determine walkability:
;;1. Check if the start is the same as the destination, if so, then true.
;;2. Check if the starting vertex is connected to the destination, if so, then true.
;;3. Append self to checklist.
;;4. Call function on the vertices connected to the starting vertices, filter the calls using the checklist.

"""(define (walkable? graph start dest check) 
			   (if (or (eq?  (length check) (length graph))) #f
			       (let ((res (map (lambda (x) (filter (lambda (y) (eq? dest y)) (cdr (select-vertex graph x)))) (iter-filter (select-vertex graph start) check))))
			     (if (not (null? (filter not-empty res))) #t
				 (let ((updated-check (cons  start check )))
				   (map (lambda (x) (walkable? graph x dest updated-check)) (cdr (select-vertex graph start)))))))) """

;;Try to make a subgraph out of vertices connected to the start and then fold walk-logic function to that graph.

(define (have-i-been-here? graph vertex places-walked)
  (let ((old-places (filter (lambda (x) (eq? (car vertex) x)) places-walked)))
    (if (null? old-places)
	#f
	#t)))


(define (do-i-know-this-place? graph vertex location)
  (let ((old-places (filter (lambda (x) (eq? location x)) vertex)))
    (if (null? old-places)
	#f
	#t)))

(define (delete-dupes list)
  (if (null? list) '()
      (cons (car list) (delete-dupes  (filter (lambda (x) (not (eq? x (car list)))) list)))))

"""(define (walkable graph start end)
			   (cond 
			    ((or (> start (len graph)) (> end (len graph))) #f)
			    ((eq? start end) #t)
			    ((null? (filter (lambda (x) (
			   (let ((walkable-unexplored
				  (delete-dupes (apply append (map (lambda (y) (filter (lambda (x)  (not (have-i-been-here? graph (select-vertex graph x) (select-vertex graph start))) (select-vertex graph y))) (select-vertex graph start)))))))
				  ;;(filter (lambda (x) (not (have-i-been-here? graph (select-vertex graph x) start))) (cdr (select-vertex g1 start)))))
			     (if (null? walkable-unexplored) #f
				 (car (filter (lambda (y) (not (not y)))
					      (map (lambda (x) (walkable (subgraph graph (list start)) x end)) walkable-unexplored)))
				 )))))))))"""



			 	;; mmemoizing accessible destinations (apply append (map (lambda (y) (filter (lambda (x)  (not (have-i-been-here? g1 (select-vertex g1 x) '(5 4 3 2 1)))) (select-vertex g1 y))) (select-vertex g1 4)))
;; (delete-dupes (apply append (map (lambda (y) (filter (lambda (x)  (not (have-i-been-here? g1 (select-vertex g1 x) '(5 4 3 2 1)))) (select-vertex g1 y))) (select-vertex g1 4))))


"""(define (walkable graph start end)
  (if (null? graph) #f
      (cond 
       ((or (> start (length graph)) (> end (length graph))) #f)
       ((eq? start end) #t)
       (else 
	(let ((walkable-unexplored
	       (delete-dupes (apply append (map (lambda (y) (filter (lambda (x)  (not (have-i-been-here? graph (select-vertex graph x) (select-vertex graph start)))) (select-vertex graph y))) (select-vertex graph start))))))
	  (if (null? walkable-unexplored) #f
	      (car (filter (lambda (y) (not (not y)))
			   (map (lambda (x) (walkable (subgraph-rm-v graph (list start)) x end)) walkable-unexplored)))
	      ))))))
"""

(define (walkable graph start end)
		       (define unexplored (delete-dupes (apply append (map (lambda (y) (filter (lambda (x)  (not (have-i-been-here? graph (select-vertex graph x) (select-vertex graph start)))) (select-vertex graph y))) (select-vertex graph start)))))
		       (cond ((null? graph) #f)
			     ((eq? start end) #t)
			     ((not (null? (filter (lambda (x) (eq? x end)) (select-vertex  graph start)))) #t)
			     ((or (> start (length graph)) (> end (length graph))) #f)
			     ((null? unexplored) #f) 
			     (else (walkable (subgraph-rm-v graph (select-vertex graph start)) (car unexplored) end))))
