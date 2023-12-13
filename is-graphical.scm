(define (even-number-of-odd-numbers? list)
  (define count 0)
  (map (lambda (x) (set! count (+ count (if (odd? x) 1
					0)))) list)
  (even? count)
  )

(define (subtract-front-portion list portion)
  (if (= portion 0) list (cons (- (car list) 1) (subtract-front-portion (cdr list) (- portion 1))))
  )

(define (is-graphical-Eggleton list)
  """
1. IF first degree in the nonincreasing list is greater than the size of the list, return false.
2. Check for an even number of odd number degrees. If not even, return false.
3. Sort the list to be nonincreasing if necessary. 
4. Take the first degree p in the list, and subtract 1  from each degree up to the pth degree. TODO!
5. If the list is all 0, then it is true
6. Recurse by passing the rest of the list into the function 
 """
 (sort-list list >)
 (cond 
  ((null? list) #f)
  ((not (even-number-of-odd-numbers? list)) #f)
  ((not (null? (filter (let ((y (length list))) (lambda (x ) (or (> x y) (< x 0)))) list)))  #f)
  ((= (length (filter (lambda (x) (= 0)) list)) (length list)) #t)
  (else
   (let ((newlist (subtract-front-portion list (car list))))
     (is-graphical-Eggleton (cdr newlist)))
   )))

(define (ErdosGallai-sum list index)
  (if (= index (length list)) #t
      (let ((k (+ index 1)))
	(if (> (apply + (list-head list k)) (+ (* k index) (apply + (map (lambda (x) (min k x)) (list-tail list k)))))
	    #f (ErdosGallai-sum list (+ index 1))))))

(define (is-graphical-ErdosGallai list)
  (sort-list list >)
  (cond
   ((null? list) #f)
   ((not (even-number-of-odd-numbers? list)) #f)
   ((not (null? (filter (let ((y (length list))) (lambda (x ) (or (> x y) (< x 0)))) list)))  #f)
   (else (ErdosGallai-sum list 0))
   )
  )
