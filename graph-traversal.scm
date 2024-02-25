
(load "graph.scm")

(define (select-vertex graph vertex-value)
  (let ((vertex (filter (lambda (x) (eq? (car x)  vertex-value)) graph)))
    (if (null? vertex) '() (car vertex)))
  )

(define (walkable? graph start dest)
  (if (eq? start dest) '() ( (map (lambda (x) (select-vertex graph x)) (cdr (select-vertex graph start)))  (map (lambda (x) (select-vertex graph x)) (cdr (select-vertex graph dest))))))


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
