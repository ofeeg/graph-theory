(load "is-graphical.scm")

(define (vertex value vertices)
  (cons value vertices)
  )

(define (create-graph degseq)
  (if (is-graphical-ErdosGallai degseq) #f
      #t
      )
  )


;;TODO: This doesn't work, but is a good starting point.
(define (connect-func vertex vertices)
  (if (list? (car vertices)) (set-cdr! vertex (append (cdr vertex) (list (map car vertices)))) (set-cdr! vertex (append (cdr vertex) vertices)))
  )

(define (iter-filter list flist)
			   (if (null? flist) list (iter-filter (filter (let ((y (car flist))) (lambda (x) (not (eq? y x)))) list) (cdr flist)))
			   )
;;This doesn't work.
(define (connect-func vertex vertices)
  (if (list? (car vertices)) (set-cdr! vertex (append (cdr vertex) (iter-filter (list (map car vertices)) (cdr vertex)))) (set-cdr! vertex (append (cdr vertex) (iter-filter vertices (cdr vertex)))))
  (map (let ((y vertex)) (lambda (x) (connect-func x y))) vertices)
  )
