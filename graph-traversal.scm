
(load "graph.scm")

(define (select-vertex graph vertex-value)
  (let ((vertex (filter (lambda (x) (eq? (car x)  vertex-value)) graph)))
    (if (null? vertex) '() (car vertex)))
  )


(define (not-empty list) (not (null? list)))


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


(define (walkable graph start end)
  ;;To determine walkability:
  ;;1. Check if the start is the same as the destination, if so, then true.
  ;;2. Check if the starting vertex is connected to the destination, if so, then true.
  ;;3. Check for valid input.
  ;;4. Check if there are any destinations not connected to the starting location.
  ;;5. If none, false. If some, then create a subgraph excluding the vertices in the starting location and recursively call self.
  (define unexplored
    (delete-dupes (apply append
			 (map (lambda (y) (filter
				       (lambda (x)  (not (have-i-been-here? graph (select-vertex graph x) (select-vertex graph start))))
				       (select-vertex graph y))) (select-vertex graph start)))))
  (cond ((null? graph) #f)
	((eq? start end) #t)
	((not (null? (filter (lambda (x) (eq? x end)) (select-vertex  graph start)))) #t)
	((or (> start (length graph)) (> end (length graph))) #f)
	((null? unexplored) #f) 
	(else (walkable (subgraph-rm-v graph (select-vertex graph start)) (car unexplored) end))))


(define (path? graph vertices)
  ;;A path is a list of vertices x to z where each vertex in between connects to eachother through to the end, and there are no duplicates.
  ;;Check for valid input, check if the next vertex in the list is actually connected to the first, and if so recurse through to the end.
  (if (or (null? vertices) (null? (cdr vertices))  (null? graph)) #t
   (let ((path (map (lambda (x) (select-vertex graph x)) vertices)))
  (cond ((< 1 (length (filter (lambda (x) (eq? (car x) (car vertices))) path))) #f)
	((null? (filter (lambda (x) (eq? (cadr vertices) x)) (select-vertex graph (car vertices)))) #f)
	(else (path? graph (cdr vertices)))))))

(define (circuit? graph vertices)
  ;; A circuit is just a path with the first vertex added at the end.
  ;; Check for valid input, if the first and last vertex match, and check if the rest, excluding the end, is a valid path.
  (if (and (equal? (list-tail vertices (1- (length vertices))) (list-head vertices 1))
	   (> (length vertices) 2))
      (path? graph (list-head vertices (1- (length vertices)))) #f))
